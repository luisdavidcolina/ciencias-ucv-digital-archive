// ==========================================================================
// BÚSQUEDA Y RENDER — ARCHIVO INSTITUCIONAL
// ==========================================================================

let _archivoSearchSeq = 0;
let _archivoAbortController = null;
let _archivoModalTrigger = null;

// app.js llama a esta función (mismo patrón que _debouncedRrhhSearch en
// hr.js) para el input de búsqueda; sin ella el guard `typeof === "function"`
// caía en triggerArchivoSearch() directo y cada tecla disparaba una petición.
const _debouncedArchivoSearch = (() => {
  let timer;
  return () => { clearTimeout(timer); timer = setTimeout(triggerArchivoSearch, 420); };
})();

// VI-031: normaliza un campo de clasificación que puede llegar como array,
// como cadena simple, o como un array de Python vuelto texto
// (`"['Presupuesto', 'Consejo de Facultad']"`) — el caso real observado en
// producción. Sin esto se pinta el literal completo, corchetes y comillas
// incluidos, dentro de una sola insignia.
function _normalizeTesauroTerms(val) {
  if (Array.isArray(val)) return val.filter(Boolean);
  if (val == null) return [];
  const s = String(val).trim();
  if (/^\[.*\]$/.test(s)) {
    const items = s.slice(1, -1).match(/'([^']*)'|"([^"]*)"/g);
    if (items) return items.map(m => m.slice(1, -1)).filter(Boolean);
  }
  return s ? [s] : [];
}

// VI-005/VI-006: soporte y tipología son texto libre sin longitud máxima en
// la base; sin recorte, una insignia larga desborda la tarjeta (contenedor
// flex sin max-width) y en 390px se sale del viewport. Recorte por JS con
// `title` con el valor completo, para no depender de que la clase CSS de la
// insignia ya tenga max-width/ellipsis.
function _truncBadge(text, max = 28) {
  const s = String(text || "");
  return s.length > max ? s.slice(0, max - 1).trimEnd() + "…" : s;
}

// Esqueleto de carga: isomorfo a la tarjeta real (BA-053) y con tantos
// bloques como perPage, para no dar un salto de altura al llegar los datos.
function showArchivoSkeleton() {
  const container = document.getElementById("list_archivo");
  if (container) {
    // BA-072: el esqueleto no son `listitem`; sin quitar el rol, un lector de
    // pantalla oiría "lista" para un contenido que no lo es.
    container.removeAttribute("role");
    const count = Math.min(state.archivo.perPage || 10, 12);
    container.innerHTML = Array.from({ length: count }, () => `
      <div class="ds-item-card ds-skeleton-card" aria-hidden="true">
        <div class="ds-item-thumbnail"><div class="ds-skeleton" style="width:40px;height:50px;"></div></div>
        <div class="ds-item-metadata">
          <div class="ds-skeleton mb-2" style="width:30%;height:16px;"></div>
          <div class="ds-skeleton mb-2" style="width:80%;height:20px;"></div>
          <div class="ds-skeleton mb-1" style="width:50%;height:13px;"></div>
          <div class="ds-skeleton"      style="width:40%;height:13px;"></div>
        </div>
      </div>`).join("");
  }
  _renderArchivoFacetsSkeleton();
}

function _renderArchivoFacetsSkeleton() {
  const el = document.getElementById("archivo-facets-panel");
  if (!el) return;
  el.innerHTML = `
    <div class="card card-secondary mt-2" aria-hidden="true">
      <div class="card-body p-2">
        <div class="ds-skeleton mb-2" style="width:70%;height:12px;"></div>
        <div class="ds-skeleton mb-1" style="width:100%;height:20px;"></div>
        <div class="ds-skeleton mb-1" style="width:100%;height:20px;"></div>
        <div class="ds-skeleton" style="width:100%;height:20px;"></div>
      </div>
    </div>`;
}

function _renderArchivoError(message) {
  const container = document.getElementById("list_archivo");
  if (!container) return;
  container.removeAttribute("role"); // BA-072: el aviso de error no es una lista
  container.innerHTML = `
    <div class="alert alert-danger text-center p-4">
      <i class="fas fa-exclamation-triangle fa-2x mb-2"></i>
      <p class="mb-2">${escHtml(message || "No se pudo cargar el archivo. Compruebe su conexión.")}</p>
      <button type="button" class="btn btn-sm btn-outline-danger" onclick="triggerArchivoSearch()">Reintentar</button>
    </div>`;
  const countEl = document.getElementById("count-archivo-results");
  if (countEl) countEl.innerText = "Error al cargar";
  // VI-069: antes se vaciaba por completo (`innerHTML = ""`), y la columna de
  // facetas perdía su marco de tarjeta —la sección cambiaba de forma sin
  // explicación—. Ahora conserva el mismo contenedor con su propio aviso.
  const facetsEl = document.getElementById("archivo-facets-panel");
  if (facetsEl) {
    facetsEl.innerHTML = `
      <div class="card card-secondary mt-2">
        <div class="card-body p-2 text-center text-muted">
          <i class="fas fa-exclamation-circle mb-1" aria-hidden="true"></i>
          <p class="mb-0" style="font-size:0.8rem;">No se pudieron cargar los filtros.</p>
        </div>
      </div>`;
  }
}

async function triggerArchivoSearch() {
  showArchivoSkeleton();
  const seq = ++_archivoSearchSeq;
  if (_archivoAbortController) _archivoAbortController.abort();
  const controller = new AbortController();
  _archivoAbortController = controller;

  const listEl  = document.getElementById("list_archivo");
  const countEl = document.getElementById("count-archivo-results");
  if (listEl)  listEl.setAttribute("aria-busy", "true");
  if (countEl) countEl.innerText = "Buscando…";

  try {
    const res = await fetch(`${API_BASE}/api/archivo/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        search_term:   state.archivo.search,
        doc_types:     state.archivo.selectedTypes,
        tesauro_terms: state.archivo.selectedTesauro,
        date_start:    state.archivo.dateStart,
        date_end:      state.archivo.dateEnd,
        sort_mode:     state.archivo.sortMode,
        soporte:       document.getElementById("soporte_archivo")?.value || "",
        page:          state.archivo.page,
        per_page:      state.archivo.perPage
      }),
      signal: controller.signal
    });
    if (seq !== _archivoSearchSeq) return;
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    // Retrocompatibilidad: si la respuesta es un array (formato viejo), tratar como antes
    if (Array.isArray(data)) {
      state.archivo.results = data;
      state.archivo.total   = data.length;
    } else {
      state.archivo.results = data.records || [];
      state.archivo.total   = data.total   || state.archivo.results.length;
    }
    // BA-021: un offset más allá del final devuelve total=0 sin filas, y la
    // pantalla mentiría "0 resultados". Si eso pasa con page>1, se vuelve a
    // la página 1 (siempre válida) en vez de mostrar el vacío falso.
    if (!Array.isArray(data) && (data.total || 0) === 0 && state.archivo.page > 1) {
      state.archivo.page = 1;
      if (listEl) listEl.removeAttribute("aria-busy");
      triggerArchivoSearch();
      return;
    }
    renderArchivoList();
    renderArchivoPagination();
    _renderArchivoFacets(data.facets || null);
  } catch (e) {
    if (e.name === "AbortError" || seq !== _archivoSearchSeq) return;
    console.error("Error buscando archivo:", e);
    _renderArchivoError();
    showToast("No se pudo cargar el archivo. Intente de nuevo.", "error");
  } finally {
    if (seq === _archivoSearchSeq && listEl) listEl.removeAttribute("aria-busy");
  }
}

function renderArchivoPagination() {
  const container = document.getElementById("archivo-pagination");
  if (!container) return;
  const total   = state.archivo.total || state.archivo.results.length;
  const perPage = state.archivo.perPage || 10;
  const page    = state.archivo.page    || 1;
  const pages   = Math.ceil(total / perPage) || 1;
  if (pages <= 1) { container.innerHTML = ""; return; }
  const winSize   = 5;
  const startPage = Math.max(1, Math.min(page - Math.floor(winSize / 2), pages - winSize + 1));
  const endPage   = Math.min(pages, startPage + winSize - 1);
  const pageNums  = [];
  for (let i = startPage; i <= endPage; i++) pageNums.push(i);

  container.innerHTML = `
    <nav class="mt-3 d-flex align-items-center justify-content-center" aria-label="Paginación de resultados">
      <ul class="pagination pagination-sm mb-0">
        <li class="page-item ${page <= 1 ? "disabled" : ""}">
          <button class="page-link" type="button" onclick="changeArchivoPage(1)" ${page <= 1 ? 'disabled aria-disabled="true"' : ""} aria-label="Primera página"><i class="fas fa-angle-double-left" aria-hidden="true"></i></button>
        </li>
        <li class="page-item ${page <= 1 ? "disabled" : ""}">
          <button class="page-link" type="button" onclick="changeArchivoPage(${page - 1})" ${page <= 1 ? 'disabled aria-disabled="true"' : ""} aria-label="Página anterior"><i class="fas fa-chevron-left" aria-hidden="true"></i></button>
        </li>
        ${pageNums.map(p => `<li class="page-item ${p === page ? "active" : ""}">
          <button class="page-link" type="button" onclick="changeArchivoPage(${p})" aria-label="Página ${p}"${p === page ? ' aria-current="page"' : ""}>${p}</button>
        </li>`).join("")}
        <li class="page-item ${page >= pages ? "disabled" : ""}">
          <button class="page-link" type="button" onclick="changeArchivoPage(${page + 1})" ${page >= pages ? 'disabled aria-disabled="true"' : ""} aria-label="Página siguiente"><i class="fas fa-chevron-right" aria-hidden="true"></i></button>
        </li>
        <li class="page-item ${page >= pages ? "disabled" : ""}">
          <button class="page-link" type="button" onclick="changeArchivoPage(${pages})" ${page >= pages ? 'disabled aria-disabled="true"' : ""} aria-label="Última página"><i class="fas fa-angle-double-right" aria-hidden="true"></i></button>
        </li>
      </ul>
    </nav>`;
}

function changeArchivoPage(p) {
  const total   = state.archivo.total || state.archivo.results.length;
  const pages   = Math.max(1, Math.ceil(total / (state.archivo.perPage || 10)));
  state.archivo.page = Math.max(1, Math.min(p, pages));
  triggerArchivoSearch();
  const header = document.querySelector(".ds-results-header");
  if (header) header.scrollIntoView({ behavior: _motionBehavior(), block: "start" });
}

function getDocumentIcon(docType) {
  const dt = (docType || "").toLowerCase();
  if (dt.includes("plano")) {
    return { icon: "fas fa-drafting-compass", color: "#17a2b8" }; // teal
  }
  if (dt.includes("informe") || dt.includes("reporte")) {
    return { icon: "fas fa-file-contract", color: "#28a745" }; // green
  }
  if (dt.includes("acta") || dt.includes("resolución") || dt.includes("resolucion")) {
    return { icon: "fas fa-gavel", color: "#dc3545" }; // red
  }
  if (dt.includes("reglamento") || dt.includes("manual") || dt.includes("guía") || dt.includes("guia")) {
    return { icon: "fas fa-book", color: "#6f42c1" }; // purple
  }
  if (dt.includes("convenio")) {
    return { icon: "fas fa-handshake", color: "#fd7e14" }; // orange
  }
  return { icon: "fas fa-file-alt", color: "#2b4e72" }; // default blue-grey
}

function hasArchivoActiveFilters() {
  const s = state.archivo;
  return !!(
    (s.search && s.search.trim()) ||
    (s.selectedTypes && s.selectedTypes.length) ||
    (s.selectedTesauro && s.selectedTesauro.length) ||
    (document.getElementById("soporte_archivo")?.value) ||
    _archivoDateFilterActive()
  );
}

function _archivoDateFilterActive() {
  const lim = state.choices?.archivo;
  if (!lim) return false;
  return state.archivo.dateStart !== lim.min_date || state.archivo.dateEnd !== lim.max_date;
}

function _clearArchivoFilters() {
  document.getElementById("btn_clear_archivo")?.click();
}

function renderArchivoList() {
  const container = document.getElementById("list_archivo");
  if (!container) return;
  const results   = state.archivo.results;
  const total     = state.archivo.total || results.length;
  const hasFilter = hasArchivoActiveFilters();
  const perPage    = state.archivo.perPage || 10;
  const totalPages = Math.max(1, Math.ceil(total / perPage));

  const countEl = document.getElementById("count-archivo-results");
  if (countEl) {
    countEl.innerText = `${plural(total, hasFilter ? "Resultado" : "Registro", hasFilter ? "Resultados" : "Registros")} — Pág. ${state.archivo.page} de ${totalPages}`;
  }

  if (results.length === 0) {
    // BA-072: el aviso de "sin resultados" es una alerta, no un elemento de
    // lista; el rol se retira mientras esté presente.
    container.removeAttribute("role");
    const emptyMsg = hasFilter
      ? "No se encontraron documentos con los filtros aplicados."
      : "El archivo no contiene documentos publicados que cumplan estos criterios.";
    container.innerHTML = `
      <div class="alert alert-secondary text-center p-4">
        <i class="fas fa-folder-open fa-2x mb-2 text-muted"></i>
        <p class="mb-2">${emptyMsg}</p>
        ${hasFilter ? `<button type="button" class="btn btn-sm btn-outline-secondary" onclick="_clearArchivoFilters()">Limpiar filtros</button>` : ""}
      </div>`;
    return;
  }

  // BA-072: sólo aquí el contenido son de verdad tarjetas equivalentes a
  // elementos de lista, así que sólo aquí se declara `role="list"`.
  container.setAttribute("role", "list");

  const searchTerms = (state.archivo.search || "").trim().split(/\s+/).filter(t => t.length > 1);

  container.innerHTML = results.map(doc => {
    const iconData = getDocumentIcon(doc.doc_type);
    const hl = txt => typeof highlightTerms === "function" ? highlightTerms(txt, searchTerms) : (escHtml(txt) || "");
    const hasFile = !!(doc.file_url);

    // Una sola insignia de soporte; tener fichero se expresa con el icono de
    // acción de "Abrir archivo digital", no con una segunda insignia (BA-045).
    const soporteIcon  = { "Físico": "fa-archive", "Digital": "fa-laptop", "Digitalizado": "fa-print" }[doc.soporte] || "fa-archive";
    const soporteLabel = doc.soporte || (hasFile ? "Digital" : "");

    // Sin el `|| []`, un registro sin tesauro_badges lanza dentro del .map() y
    // se queda en blanco la lista entera, no solo esa tarjeta.
    const badges = (doc.tesauro_badges || [])
      .filter(b => b !== doc.tesauro_primario && b !== doc.doc_type);

    const titleText = escHtml(doc.titulo || "Documento sin título");

    return `
    <div class="ds-item-card" role="listitem" onclick="openArchivoModal(${doc.__idx})" style="cursor:pointer;">
      <div class="ds-item-thumbnail">
        <i class="${iconData.icon}" style="color:${iconData.color};" aria-hidden="true"></i>
        ${soporteLabel ? `<span class="badge ds-badge mt-1" style="font-size:0.6rem;max-width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${escHtml(soporteLabel)}"><i class="fas ${soporteIcon} mr-1" aria-hidden="true"></i>${escHtml(_truncBadge(soporteLabel))}</span>` : ""}
      </div>
      <div class="ds-item-metadata">
        <button type="button" class="ds-item-title btn btn-link p-0 text-left" onclick="event.stopPropagation();openArchivoModal(${doc.__idx})">${hl(doc.titulo)}</button>
        <div class="d-flex justify-content-between align-items-center flex-wrap" style="gap:6px;">
          <span class="badge ds-badge" style="margin-top:0;max-width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${escHtml(doc.tesauro_primario || doc.doc_type)}"><i class="fas fa-bookmark mr-1" aria-hidden="true"></i>${escHtml(_truncBadge(doc.tesauro_primario || doc.doc_type))}</span>
          ${doc.fecha ? `<span class="text-muted" style="font-size:0.8rem;"><i class="far fa-calendar-alt mr-1" aria-hidden="true"></i>${escHtml(formatISOToSpanish(doc.fecha))}</span>` : ""}
        </div>
        ${doc.autor ? `<div class="ds-item-authors"><i class="fas fa-user-edit mr-1" aria-hidden="true"></i>${hl(doc.autor)}</div>` : ""}
        ${(doc.ubicacion || doc.numero_folio || doc.numero_paginas) ? `<div class="ds-item-publisher">
          ${doc.ubicacion ? `<i class="fas fa-map-marker-alt mr-1" aria-hidden="true"></i>${escHtml(doc.ubicacion)}` : ""}
          ${doc.numero_folio ? `<span class="ml-2"><i class="fas fa-hashtag mr-1" aria-hidden="true"></i>${escHtml(doc.numero_folio)}</span>` : ""}
          ${doc.numero_paginas ? `<span class="ml-2"><i class="fas fa-file-alt mr-1" aria-hidden="true"></i>${escHtml(String(doc.numero_paginas))} p.</span>` : ""}
        </div>` : ""}
        ${doc.resumen ? `<p class="ds-item-abstract m-0">${hl(doc.resumen)}</p>` : ""}
        <div class="ds-item-badges d-flex flex-wrap mt-2" style="gap:4px;">
          ${badges.slice(0, 4).map(b => `<span class="badge ds-kw-badge">${escHtml(b)}</span>`).join("")}
          ${badges.length > 4 ? `<span class="badge ds-kw-badge ds-kw-badge-more">+${badges.length - 4}</span>` : ""}
        </div>
      </div>
      <div class="ds-item-actions">
        <button class="btn btn-primary ds-action-btn" aria-label="Ver detalle de «${titleText}»"
          onclick="event.stopPropagation();openArchivoModal(${doc.__idx})">
          <i class="fas fa-eye" aria-hidden="true"></i>
        </button>
        ${hasFile ? `<a href="${_secureFileUrl(doc.file_url)}" target="_blank" rel="noopener" class="btn btn-outline-info ds-action-btn" aria-label="Abrir archivo digital de «${titleText}»"
          onclick="event.stopPropagation()">
          <i class="fas fa-file-pdf" style="font-size:0.8rem;" aria-hidden="true"></i>
        </a>` : ""}
        ${(state.user && state.user.roles && state.user.roles.Archivo === "Admin") ? `<a href="/admin/archivo?docId=${doc.id}" class="btn btn-outline-warning ds-action-btn" aria-label="Editar documento «${titleText}» (Admin)"
          onclick="event.stopPropagation()">
          <i class="fas fa-pen" style="font-size:0.8rem;" aria-hidden="true"></i>
        </a>` : ""}
      </div>
    </div>
    `;
  }).join("");
}

function openArchivoModal(idxReal) {
  const doc = state.archivo.results.find(d => d.__idx == idxReal);
  if (!doc) return;
  _archivoModalTrigger = document.activeElement;
  openDocModalWithRecord(doc);
}

function openDocModalWithRecord(doc) {
  const iconData = getDocumentIcon(doc.doc_type);
  document.getElementById("modal-doc-title").innerText       = doc.titulo;
  document.getElementById("modal-doc-thumb-icon").className  = iconData.icon;
  document.getElementById("modal-doc-thumb-icon").style.color = iconData.color;
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type || "";

  const isActa   = /^acta|^resoluc/i.test(doc.doc_type);
  const isPlano  = /plano/i.test(doc.doc_type);
  const anio     = (doc.fecha || "").substring(0, 4);
  // VI-031: cuando no llega `tesauro_badges`, el respaldo usaba
  // `doc.tesauro_secundario` a pelo. A veces ese campo llega como una lista
  // ya serializada a texto por Python (`str(list)`, comillas simples y
  // corchetes) en vez de una cadena limpia, y se pintaba literal:
  // `['Presupuesto', 'Consejo de Facultad']`. Se normaliza a array de
  // términos sueltos antes de unirlos.
  const badges   = (doc.tesauro_badges && doc.tesauro_badges.length)
    ? doc.tesauro_badges
    : [doc.doc_type, ..._normalizeTesauroTerms(doc.tesauro_secundario)].filter(Boolean);

  // VI-060: el título ya va en la cabecera del modal (`modal-doc-title`,
  // arriba); repetirlo como primera fila de la tabla de Metadata sólo suma
  // ocho líneas sin dato nuevo.
  if (isPlano) {
    document.getElementById("modal-doc-meta-container").innerHTML = `
      <div class="ds-doc-meta-row"><span class="k">Proyecto</span><span class="v">${escHtml(doc.proyecto || doc.titulo)}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Año</span><span class="v">${escHtml(anio || "N/A")}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Tipología</span><span class="v">${escHtml(doc.doc_type)}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Dibujante / Autor</span><span class="v">${escHtml(doc.autor || "N/A")}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${escHtml(doc.ubicacion)}</span></div>
    `;
  } else {
    const dateLabel = isActa ? "Fecha de Sesión" : "Fecha de Emisión";
    const dateValue = isActa
      ? `${escHtml(formatISOToSpanish(doc.fecha))} <small class="text-muted">(Sesión)</small>`
      : escHtml(formatISOToSpanish(doc.fecha));
    document.getElementById("modal-doc-meta-container").innerHTML = `
      <div class="ds-doc-meta-row"><span class="k">Autor / Ente</span><span class="v">${escHtml(doc.autor)}</span></div>
      <div class="ds-doc-meta-row"><span class="k">${escHtml(dateLabel)}</span><span class="v">${dateValue}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Tipología</span><span class="v">${escHtml(doc.doc_type)}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Soporte</span><span class="v">${escHtml(doc.soporte || "Físico")}</span></div>
      ${doc.numero_folio ? `<div class="ds-doc-meta-row"><span class="k">N° de Folio / Signatura</span><span class="v">${escHtml(doc.numero_folio)}</span></div>` : ""}
      ${doc.numero_paginas ? `<div class="ds-doc-meta-row"><span class="k">N° de Páginas</span><span class="v">${escHtml(String(doc.numero_paginas))}</span></div>` : ""}
      <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${escHtml(doc.ubicacion)}</span></div>
      ${doc.personas_relacionadas ? `<div class="ds-doc-meta-row"><span class="k">Personas Relacionadas</span><span class="v">${escHtml(doc.personas_relacionadas)}</span></div>` : ""}
      <div class="ds-doc-meta-row"><span class="k">Clasificación / Palabras Clave</span><span class="v">${escHtml(badges.join("; "))}</span></div>
    `;
  }
  document.getElementById("modal-doc-abstract").innerText =
    doc.resumen || "No se ha registrado un resumen descriptivo abstracto (dc.description.abstract) para este folio.";

  closeDocViewer();
  const fileUrl = _secureFileUrl(doc.file_url || "");
  const viewBtn = document.getElementById("btn-modal-view");
  // BA-067: "Ver" sólo existe como botón cuando hay una acción de archivo real
  // que ejecutar (abrir el visor). La ubicación física ya se muestra como fila
  // de metadata ("Ubicación Física") en la tabla de arriba: repetirla aquí
  // como un botón que sólo lanza un `toast` es un quinto significado bajo el
  // mismo control, no una acción. Sin fichero, el botón se oculta.
  if (fileUrl) {
    const isImg = /\.(png|jpe?g|gif|webp|svg)$/i.test(fileUrl);
    const isPdf = /\.pdf$/i.test(fileUrl);
    const viewIcon = isImg ? "fas fa-image" : isPdf ? "fas fa-file-pdf" : "fas fa-external-link-alt";
    const viewText = isImg ? "Ver Imagen" : isPdf ? "Ver PDF" : "Abrir Archivo";
    viewBtn.innerHTML = `<i class="${viewIcon} mr-1"></i>${viewText}`;
    viewBtn.onclick = () => toggleDocViewer(fileUrl);
    viewBtn.classList.remove("d-none");
  } else {
    viewBtn.onclick = null;
    viewBtn.classList.add("d-none");
  }
  const editBtn = document.getElementById("btn-modal-edit");
  // VI-061: el botón traía sólo el lápiz (aria-label sí, texto visible no) y
  // quedaba indistinguible de un adorno junto a «Cerrar»/«Ver Imagen», que sí
  // llevan texto. Mismo patrón que `viewBtn` arriba: icono + etiqueta.
  editBtn.innerHTML = '<i class="fas fa-pen mr-1" aria-hidden="true"></i>Editar';
  if (state.user && state.user.roles && state.user.roles.Archivo === "Admin" && doc.id) {
    editBtn.classList.remove("d-none");
    editBtn.onclick = () => { window.location.href = `/admin/archivo?docId=${doc.id}`; };
  } else {
    editBtn.classList.add("d-none");
  }

  if (typeof $ === "undefined" || !$.fn || !$.fn.modal) {
    console.error("jQuery/Bootstrap modal no disponible");
    showToast("No se pudo abrir el detalle: los recursos de la página no cargaron. Recargue e intente de nuevo.", "error");
    return;
  }
  $("#doc-modal").modal("show");
}

if (typeof $ !== "undefined" && $.fn && $.fn.modal) {
  // BA-076: Bootstrap sólo devuelve el foco al disparador si el modal se abrió
  // desde un elemento declarado con data-toggle; aquí se abre por JS, así que
  // el foco se restaura a mano al cerrar.
  $("#doc-modal").on("hidden.bs.modal", () => {
    if (_archivoModalTrigger && typeof _archivoModalTrigger.focus === "function") {
      _archivoModalTrigger.focus();
    }
    _archivoModalTrigger = null;
  });
}

// toggleDocViewer()/closeDocViewer() viven en app-core.js: hr.js (dossier de
// RRHH) los necesita sobre el mismo markup compartido (#modal-doc-viewer-section
// / #modal-doc-iframe, idéntico en archive.html y hr.html) y hr.html no carga
// este archivo. Antes vivían aquí solos y cada apertura del modal de
// documento RRHH lanzaba un ReferenceError sin capturar (BUSQUEDA-llamadas-huerfanas).

// ==========================================================================
// FACETAS DE BÚSQUEDA — conteos por tipo y por año
// ==========================================================================
function _renderArchivoFacets(facets) {
  const el = document.getElementById("archivo-facets-panel");
  if (!el) return;
  if (!facets || (!facets.by_type?.length && !facets.by_year?.length)) {
    el.innerHTML = "";
    return;
  }
  const byType = (facets.by_type || []).slice(0, 10);
  const byYear = (facets.by_year || []).slice(0, 8);
  const selectedTypes = state.archivo.selectedTypes || [];

  const typeRows = byType.map(f => {
    const active = selectedTypes.includes(f.name);
    // BA-016: el backend etiqueta la faceta sin tipo con el centinela
    // "__sin_tipo__" (no con el texto "Sin tipo") para poder traducirla de
    // vuelta a `tesauro_primario = ''`; aquí sólo se decide qué se muestra.
    const label = f.name === "__sin_tipo__" ? "Sin tipo" : f.name;
    return `<button type="button" class="ds-facet-row w-100 d-flex justify-content-between align-items-center py-1 px-1${active ? " ds-facet-active" : ""}"
                 data-facet-type="${escHtml(f.name)}" aria-pressed="${active}"
                 style="font-size:0.78rem;border:none;background:transparent;text-align:left;">
      <span class="text-truncate" style="max-width:140px;" title="${escHtml(label)}">${escHtml(label)}</span>
      <span class="badge badge-secondary ml-1" style="font-size:0.68rem;min-width:24px;text-align:center;">${f.count}</span>
    </button>`;
  }).join("");

  const yearRows = byYear.map(f =>
    `<button type="button" class="ds-facet-row w-100 d-flex justify-content-between align-items-center py-1 px-1"
          data-facet-year="${f.year}" aria-pressed="false"
          style="font-size:0.78rem;border:none;background:transparent;text-align:left;">
      <span>${f.year}</span>
      <span class="badge badge-secondary ml-1" style="font-size:0.68rem;min-width:24px;text-align:center;">${f.count}</span>
    </button>`
  ).join("");

  el.innerHTML = `
    <div class="card card-secondary mt-2" style="font-size:0.82rem;">
      <div class="card-header py-1 px-2" style="background:#f4f9ff;">
        <span class="font-weight-bold text-primary" style="font-size:0.8rem;"><i class="fas fa-chart-bar mr-1" aria-hidden="true"></i>Distribución</span>
      </div>
      <div class="card-body p-2">
        ${byType.length ? `<p class="text-muted mb-1" style="font-size:0.72rem;text-transform:uppercase;letter-spacing:.04em;">Por Tipo</p>${typeRows}` : ""}
        ${byYear.length ? `<p class="text-muted mb-1 mt-2" style="font-size:0.72rem;text-transform:uppercase;letter-spacing:.04em;">Por Año</p>${yearRows}` : ""}
      </div>
    </div>`;

  // BA-001/BA-070: delegación de eventos por data-*, nada de handlers en
  // línea con nombres inyectados por JSON.stringify (rompía con comillas en
  // el atributo) ni divs sin semántica de botón (inalcanzables por teclado).
  if (!el.dataset.delegated) {
    el.dataset.delegated = "1";
    el.addEventListener("click", e => {
      const typeBtn = e.target.closest("[data-facet-type]");
      if (typeBtn) { _facetTypeClick(typeBtn.dataset.facetType); return; }
      const yearBtn = e.target.closest("[data-facet-year]");
      if (yearBtn) { _facetYearClick(yearBtn.dataset.facetYear); }
    });
  }
}

function _facetTypeClick(name) {
  if (!state.archivo.selectedTypes) state.archivo.selectedTypes = [];
  const idx = state.archivo.selectedTypes.indexOf(name);
  if (idx >= 0) state.archivo.selectedTypes.splice(idx, 1);
  else state.archivo.selectedTypes.push(name);
  // BA-013: el proyecto usa TomSelect (tsInstances), no Choices.js.
  const ts = tsInstances["choice-archivo-doc-type"];
  if (ts) ts.setValue(state.archivo.selectedTypes, true);
  state.archivo.page = 1;
  triggerArchivoSearch();
}

function _facetYearClick(year) {
  const y = String(year);
  // BA-012: activar la pestaña "Año" del acordeón y su panel, no sólo cambiar
  // el estado interno, para que el usuario vea de dónde salió el filtro.
  if (typeof applyDatePreset === "function") applyDatePreset("archivo", "year");
  state.archivo.dateStart = y + "-01-01";
  state.archivo.dateEnd   = y + "-12-31";
  const ys = document.getElementById("year-select-archivo");
  if (ys) ys.value = y;
  const lbl = document.getElementById("fp-archivo-label");
  if (lbl) lbl.innerText = `Año ${y}`;
  state.archivo.page = 1;
  triggerArchivoSearch();
}

// BA-187: como los colores de la tarjeta viven en el marcado generado, un
// cambio de tema con la lista ya pintada dejaba las tarjetas con los colores
// anteriores hasta la siguiente búsqueda.
document.addEventListener("ds:theme-change", () => {
  if (state.archivo.results && state.archivo.results.length) renderArchivoList();
});
