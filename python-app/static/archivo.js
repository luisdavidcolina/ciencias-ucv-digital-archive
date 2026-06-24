// ==========================================================================
// BÚSQUEDA Y RENDER — ARCHIVO INSTITUCIONAL
// ==========================================================================

// Skeleton de carga para resultados
function showArchivoSkeleton() {
  const container = document.getElementById("list_archivo");
  if (!container) return;
  container.innerHTML = Array.from({ length: 4 }, () => `
    <div class="ds-item-card" style="pointer-events:none;opacity:0.7;">
      <div class="ds-item-thumbnail"><div style="width:40px;height:50px;background:#e9ecef;border-radius:6px;"></div></div>
      <div class="ds-item-metadata" style="flex-grow:1;padding-left:15px;">
        <div class="ds-skeleton mb-2" style="width:30%;height:16px;"></div>
        <div class="ds-skeleton mb-2" style="width:80%;height:20px;"></div>
        <div class="ds-skeleton mb-1" style="width:50%;height:13px;"></div>
        <div class="ds-skeleton"      style="width:40%;height:13px;"></div>
      </div>
    </div>`).join("");
}

const _debouncedArchivoSearch = (() => {
  let timer;
  return () => { clearTimeout(timer); timer = setTimeout(triggerArchivoSearch, 420); };
})();

async function triggerArchivoSearch() {
  showArchivoSkeleton();
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
      })
    });
    if (!res.ok) throw new Error();
    const data = await res.json();
    // Retrocompatibilidad: si la respuesta es un array (formato viejo), tratar como antes
    if (Array.isArray(data)) {
      state.archivo.results = data;
      state.archivo.total   = data.length;
    } else {
      state.archivo.results = data.records || [];
      state.archivo.total   = data.total   || state.archivo.results.length;
    }
    renderArchivoList();
    renderArchivoPagination();
  } catch (e) {
    console.error("Error buscando archivo:", e);
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
    <nav class="mt-3 d-flex align-items-center justify-content-between flex-wrap" style="gap:6px;">
      <small class="text-muted">Pág. ${page} de ${pages} &mdash; ${total} resultados</small>
      <ul class="pagination pagination-sm mb-0">
        <li class="page-item ${page <= 1 ? 'disabled' : ''}">
          <button class="page-link" onclick="changeArchivoPage(1)" title="Primera"><i class="fas fa-angle-double-left"></i></button>
        </li>
        <li class="page-item ${page <= 1 ? 'disabled' : ''}">
          <button class="page-link" onclick="changeArchivoPage(${page - 1})"><i class="fas fa-chevron-left"></i></button>
        </li>
        ${pageNums.map(p => `<li class="page-item ${p === page ? 'active' : ''}">
          <button class="page-link" onclick="changeArchivoPage(${p})">${p}</button>
        </li>`).join("")}
        <li class="page-item ${page >= pages ? 'disabled' : ''}">
          <button class="page-link" onclick="changeArchivoPage(${page + 1})"><i class="fas fa-chevron-right"></i></button>
        </li>
        <li class="page-item ${page >= pages ? 'disabled' : ''}">
          <button class="page-link" onclick="changeArchivoPage(${pages})" title="Última"><i class="fas fa-angle-double-right"></i></button>
        </li>
      </ul>
    </nav>`;
}

function changeArchivoPage(p) {
  state.archivo.page = p;
  triggerArchivoSearch();
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

function renderArchivoList() {
  const container = document.getElementById("list_archivo");
  const results   = state.archivo.results;
  const total     = state.archivo.total || results.length;
  const hasFilter = !!(state.archivo.search || (state.archivo.selectedTypes && state.archivo.selectedTypes.length) || (state.archivo.selectedTesauro && state.archivo.selectedTesauro.length));
  document.getElementById("count-archivo-results").innerText =
    hasFilter ? `${total} Resultados` : `${total} Registros`;

  if (results.length === 0) {
    const emptyMsg = hasFilter
      ? `No se encontraron documentos con los filtros aplicados.
         <br><small class="text-muted">Intente ampliar la búsqueda o limpiar los filtros.</small>`
      : `El archivo no contiene documentos registrados aún.`;
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-folder-open fa-2x mb-2 text-muted"></i>
      <p class="mb-0">${emptyMsg}</p>
    </div>`;
    document.getElementById("info-archivo-pagination").innerText = "Pág 1 de 1";
    const prevBtn = document.getElementById("btn-archivo-prev");
    const nextBtn = document.getElementById("btn-archivo-next");
    if (prevBtn) prevBtn.disabled = true;
    if (nextBtn) nextBtn.disabled = true;
    return;
  }

  const totalPages = Math.ceil(total / state.archivo.perPage) || 1;
  document.getElementById("info-archivo-pagination").innerText = `Pág ${state.archivo.page} de ${totalPages}`;
  const prevBtnA = document.getElementById("btn-archivo-prev");
  const nextBtnA = document.getElementById("btn-archivo-next");
  if (prevBtnA) prevBtnA.disabled = state.archivo.page <= 1;
  if (nextBtnA) nextBtnA.disabled = state.archivo.page >= totalPages;

  const searchTerms = (state.archivo.search || "").trim().split(/\s+/).filter(t => t.length > 1);

  container.innerHTML = results.map(doc => {
    const iconData = getDocumentIcon(doc.doc_type);
    const hl = txt => typeof highlightTerms === "function" ? highlightTerms(txt, searchTerms) : (txt || "");
    const hasFile = !!(doc.file_url);

    const soporteIcon = {"Físico":"fa-archive","Digital":"fa-laptop","Digitalizado":"fa-scanner"}[doc.soporte] || "fa-archive";
    const soporteColor = {"Físico":"#6c757d","Digital":"#17a2b8","Digitalizado":"#fd7e14"}[doc.soporte] || "#6c757d";

    return `
    <div class="ds-item-card" onclick="openArchivoModal('${doc.__idx}')" style="cursor:pointer;border-left:3px solid ${iconData.color};">
      <div class="ds-item-thumbnail" style="display:flex;flex-direction:column;align-items:center;gap:6px;">
        <i class="${iconData.icon}" style="font-size:36px;color:${iconData.color};"></i>
        ${hasFile ? `<span class="badge badge-info" style="font-size:0.6rem;padding:2px 5px;"><i class="fas fa-file mr-1"></i>Digital</span>` : ""}
        ${doc.soporte ? `<span class="badge" style="font-size:0.55rem;padding:2px 5px;background:${soporteColor};color:#fff;"><i class="fas ${soporteIcon} mr-1"></i>${doc.soporte}</span>` : ""}
      </div>
      <div class="ds-item-metadata" style="flex-grow:1;padding-left:15px;">
        <div class="d-flex justify-content-between align-items-center mb-1">
          <span class="badge badge-light" style="font-size:0.75rem;color:#2b4e72;font-weight:bold;border:1px solid #d9e6f4;border-radius:12px;padding:3px 10px;">
            <i class="fas fa-bookmark mr-1"></i> ${doc.tesauro_primario || doc.doc_type}
          </span>
          <span class="text-muted" style="font-size:0.8rem;"><i class="far fa-calendar-alt mr-1"></i> ${formatISOToSpanish(doc.fecha)}</span>
        </div>
        <h4 class="ds-item-title" style="font-size:1.05rem;font-weight:700;color:#2b4e72;margin:4px 0;">${hl(doc.titulo)}</h4>
        <div class="ds-item-authors" style="font-size:0.82rem;color:#495057;margin-bottom:2px;">
          <i class="fas fa-user-edit mr-1"></i> <strong>${hl(doc.autor)}</strong>
        </div>
        <div class="ds-item-publisher" style="font-size:0.82rem;color:#6c757d;margin-bottom:4px;">
          <i class="fas fa-map-marker-alt mr-1"></i> <strong>${doc.ubicacion}</strong>
          ${doc.numero_folio ? `<span class="ml-2 text-muted"><i class="fas fa-hashtag mr-1"></i>${doc.numero_folio}</span>` : ""}
          ${doc.numero_paginas ? `<span class="ml-2 text-muted"><i class="fas fa-file-alt mr-1"></i>${doc.numero_paginas} p.</span>` : ""}
        </div>
        ${doc.resumen ? `<p class="ds-item-abstract text-muted m-0 mt-1" style="font-size:0.82rem;line-height:1.4;display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden;">${hl(doc.resumen)}</p>` : ""}
        <div class="ds-item-badges d-flex flex-wrap gap-1 mt-2">
          ${(() => { const filtered = doc.tesauro_badges.filter(b => b !== doc.tesauro_primario && b !== doc.doc_type); return filtered.slice(0, 4).map(b => `<span class="badge" style="background-color:#2b4e72;color:white;font-size:0.7rem;padding:2px 7px;border-radius:4px;margin-right:4px;">${b}</span>`).join(""); })()}
          ${(() => { const filtered = doc.tesauro_badges.filter(b => b !== doc.tesauro_primario && b !== doc.doc_type); return filtered.length > 4 ? `<span class="badge" style="background-color:#6c757d;color:white;font-size:0.7rem;padding:2px 7px;border-radius:4px;">+${filtered.length - 4}</span>` : ""; })()}
        </div>
      </div>
      <div class="ds-item-actions" style="margin-left:15px;display:flex;flex-direction:column;justify-content:center;gap:6px;">
        <button class="btn btn-primary ds-action-btn" title="Ver detalle"
          onclick="event.stopPropagation();openArchivoModal('${doc.__idx}')"
          style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
          <i class="fas fa-eye"></i>
        </button>
        ${hasFile ? `<a href="${doc.file_url}" target="_blank" class="btn btn-outline-info ds-action-btn" title="Abrir archivo digital"
          onclick="event.stopPropagation()"
          style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
          <i class="fas fa-file-pdf" style="font-size:0.8rem;"></i>
        </a>` : ""}
      </div>
    </div>
    `;
  }).join("");
}

function openArchivoModal(idxReal) {
  const doc = state.archivo.results.find(d => d.__idx == idxReal);
  if (!doc) return;
  openDocModalWithRecord(doc);
}

function openDocModalWithRecord(doc) {
  const iconData = getDocumentIcon(doc.doc_type);
  document.getElementById("modal-doc-title").innerText       = doc.titulo;
  document.getElementById("modal-doc-thumb-icon").className  = iconData.icon;
  document.getElementById("modal-doc-thumb-icon").style.color = iconData.color;
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type;

  const isActa   = /^acta|^resoluc/i.test(doc.doc_type);
  const isPlano  = /plano/i.test(doc.doc_type);
  const anio     = (doc.fecha || "").substring(0, 4);
  const badges   = doc.tesauro_badges || [doc.doc_type, doc.tesauro_secundario].filter(Boolean);

  if (isPlano) {
    document.getElementById("modal-doc-meta-container").innerHTML = `
      <div class="ds-doc-meta-row"><span class="k">Proyecto</span><span class="v">${doc.proyecto || doc.titulo}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Título</span><span class="v">${doc.titulo}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Año</span><span class="v">${anio || "N/A"}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Tipología</span><span class="v">${doc.doc_type}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Dibujante / Autor</span><span class="v">${doc.autor || "N/A"}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${doc.ubicacion}</span></div>
    `;
  } else {
    const dateLabel = isActa ? "Fecha de Sesión" : "Fecha de Emisión";
    const dateValue = isActa
      ? `${formatISOToSpanish(doc.fecha)} <small class="text-muted">(Sesión)</small>`
      : formatISOToSpanish(doc.fecha);
    document.getElementById("modal-doc-meta-container").innerHTML = `
      <div class="ds-doc-meta-row"><span class="k">Título</span><span class="v">${doc.titulo}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Autor / Ente</span><span class="v">${doc.autor}</span></div>
      <div class="ds-doc-meta-row"><span class="k">${dateLabel}</span><span class="v">${dateValue}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Tipología</span><span class="v">${doc.doc_type}</span></div>
      <div class="ds-doc-meta-row"><span class="k">Soporte</span><span class="v">${doc.soporte || "Físico"}</span></div>
      ${doc.numero_folio ? `<div class="ds-doc-meta-row"><span class="k">N° de Folio / Signatura</span><span class="v">${doc.numero_folio}</span></div>` : ""}
      ${doc.numero_paginas ? `<div class="ds-doc-meta-row"><span class="k">N° de Páginas</span><span class="v">${doc.numero_paginas}</span></div>` : ""}
      <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${doc.ubicacion}</span></div>
      ${doc.personas_relacionadas ? `<div class="ds-doc-meta-row"><span class="k">Personas Relacionadas</span><span class="v">${doc.personas_relacionadas}</span></div>` : ""}
      <div class="ds-doc-meta-row"><span class="k">Clasificación / Palabras Clave</span><span class="v">${badges.join("; ")}</span></div>
    `;
  }
  document.getElementById("modal-doc-abstract").innerText =
    doc.resumen || "No se ha registrado un resumen descriptivo abstracto (dc.description.abstract) para este folio.";

  closeDocViewer();
  const fileUrl = doc.file_url || "";
  const viewBtn = document.getElementById("btn-modal-view");
  if (fileUrl) {
    const isImg = /\.(png|jpe?g|gif|webp|svg)$/i.test(fileUrl);
    const isPdf = /\.pdf$/i.test(fileUrl);
    const viewIcon = isImg ? "fas fa-image" : isPdf ? "fas fa-file-pdf" : "fas fa-external-link-alt";
    const viewText = isImg ? "Ver Imagen" : isPdf ? "Ver PDF" : "Abrir Archivo";
    viewBtn.innerHTML = `<i class="${viewIcon} mr-1"></i>${viewText}`;
    viewBtn.onclick = () => toggleDocViewer(fileUrl);
  } else if ((doc.ubicacion || "").toLowerCase().includes("digitalizado")) {
    viewBtn.innerHTML = '<i class="fas fa-search mr-1"></i>Digitalizado';
    viewBtn.onclick = () => showToast("Documento digitalizado sin URL asignada. Contacte al administrador.", "warning");
  } else {
    viewBtn.innerHTML = '<i class="fas fa-map-marker-alt mr-1"></i>Ubicación';
    viewBtn.onclick = () => showToast(`Ubicación física: ${doc.ubicacion || "No registrada"}`, "info");
  }
  document.getElementById("btn-modal-download").classList.add("d-none");
  document.getElementById("btn-modal-edit").classList.add("d-none");

  $("#doc-modal").modal("show");
}

// ==========================================================================
// VISOR DE DOCUMENTO DIGITALIZADO
// ==========================================================================
function toggleDocViewer(fileUrl) {
  const section = document.getElementById("modal-doc-viewer-section");
  const iframe  = document.getElementById("modal-doc-iframe");
  if (!section || !iframe) return;
  if (section.classList.contains("d-none")) {
    const isImg = /\.(png|jpe?g|gif|webp|svg)$/i.test(fileUrl);
    if (isImg) {
      iframe.style.display = "none";
      let img = section.querySelector("img.ds-viewer-img");
      if (!img) { img = document.createElement("img"); img.className = "ds-viewer-img"; img.style.cssText = "max-width:100%;max-height:500px;display:block;margin:auto;border-radius:4px;"; section.appendChild(img); }
      img.src = fileUrl;
      img.style.display = "block";
    } else {
      const img = section.querySelector("img.ds-viewer-img");
      if (img) img.style.display = "none";
      iframe.style.display = "block";
      iframe.src = fileUrl;
    }
    section.classList.remove("d-none");
    section.scrollIntoView({ behavior: "smooth", block: "nearest" });
  } else {
    closeDocViewer();
  }
}

function closeDocViewer() {
  const section = document.getElementById("modal-doc-viewer-section");
  const iframe  = document.getElementById("modal-doc-iframe");
  if (section) section.classList.add("d-none");
  if (iframe)  iframe.src = "";
}
