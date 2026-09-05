// ==========================================================================
// BÚSQUEDA Y RENDER — RRHH
// ==========================================================================

function showRrhhSkeleton() {
  const container = document.getElementById("list_rrhh");
  if (!container) return;
  container.innerHTML = Array.from({ length: 4 }, () => `
    <div class="ds-person-card" style="pointer-events:none;opacity:0.7;">
      <div style="width:56px;height:56px;border-radius:50%;background:#e9ecef;flex-shrink:0;"></div>
      <div style="flex-grow:1;padding-left:14px;">
        <div class="ds-skeleton mb-2" style="width:60%;height:18px;"></div>
        <div class="ds-skeleton mb-2" style="width:40%;height:13px;"></div>
        <div class="ds-skeleton"      style="width:70%;height:13px;"></div>
      </div>
    </div>`).join("");
}

// VI-006: la tipología es texto libre sin longitud máxima; una insignia larga
// desborda el ancho a 390px. Recorte por JS con `title` con el valor
// completo, igual que archive.js._truncBadge, para no depender de que la
// clase CSS de la insignia ya tenga max-width/ellipsis.
function _truncBadgeRrhh(text, max = 28) {
  const s = String(text || "");
  return s.length > max ? s.slice(0, max - 1).trimEnd() + "…" : s;
}

const _debouncedRrhhSearch = (() => {
  let timer;
  return () => { clearTimeout(timer); timer = setTimeout(triggerRrhhSearch, 420); };
})();

// Descarta respuestas de búsquedas que ya no son la última lanzada (BR-018)
let _rrhhSearchSeq = 0;

async function triggerRrhhSearch() {
  if (!state.rrhh) state.rrhh = {};
  const rrhh = state.rrhh;
  showRrhhSkeleton();
  const mySeq = ++_rrhhSearchSeq;
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        search_term:  rrhh.search,
        doc_types:    rrhh.selectedTypes,
        estados:      rrhh.selectedEstados,
        people_terms: rrhh.selectedPeople,
        date_start:   rrhh.dateStart,
        date_end:     rrhh.dateEnd,
        sort_mode:    rrhh.sortMode,
        page:         rrhh.page,
        per_page:     rrhh.perPage
      })
    });
    if (mySeq !== _rrhhSearchSeq) return; // llegó una búsqueda más reciente antes
    if (!res.ok) { renderRrhhSearchError(res.status); return; }
    const data = await res.json();
    if (mySeq !== _rrhhSearchSeq) return;
    state.rrhh.results = data.records || [];
    state.rrhh.total   = data.total   || state.rrhh.results.length;
    renderRrhhList();
    renderRrhhPagination();
    _renderRrhhFacets(data.facets || null);
  } catch (e) {
    if (mySeq !== _rrhhSearchSeq) return;
    console.error("Error buscando RRHH:", e);
    renderRrhhSearchError(0);
  }
}

// Estado de error explícito en la lista, con reintento (BR-019)
function renderRrhhSearchError(status) {
  const container = document.getElementById("list_rrhh");
  if (!container) return;
  const esSesion = status === 401 || status === 403;
  const mensaje = esSesion
    ? "Su sesión expiró o no tiene permiso para consultar RRHH. Vuelva a iniciar sesión."
    : "No se pudo completar la búsqueda. Intente de nuevo.";
  container.setAttribute("role", "alert");
  container.innerHTML = `<div class="alert alert-danger text-center p-4">
    <i class="fas fa-exclamation-triangle fa-2x mb-2" aria-hidden="true"></i>
    <p class="mb-2">${escHtml(mensaje)}</p>
    ${esSesion
      ? `<a href="/login.html" class="btn btn-sm btn-primary">Iniciar sesión</a>`
      : `<button type="button" class="btn btn-sm btn-outline-danger" data-rrhh-retry>Reintentar</button>`}
  </div>`;
  const retryBtn = container.querySelector("[data-rrhh-retry]");
  if (retryBtn) retryBtn.addEventListener("click", () => { container.removeAttribute("role"); triggerRrhhSearch(); });
  showToast(mensaje, "error");
}

function renderRrhhPagination() {
  const container = document.getElementById("rrhh-pagination");
  if (!container) return;
  const total   = state.rrhh.total || state.rrhh.results.length;
  const perPage = state.rrhh.perPage || 10;
  const page    = state.rrhh.page    || 1;
  const pages   = Math.ceil(total / perPage) || 1;
  if (pages <= 1) { container.innerHTML = ""; return; }

  const winSize   = 5;
  const startPage = Math.max(1, Math.min(page - Math.floor(winSize / 2), pages - winSize + 1));
  const endPage   = Math.min(pages, startPage + winSize - 1);
  const pageNums  = [];
  for (let i = startPage; i <= endPage; i++) pageNums.push(i);

  container.innerHTML = `
    <nav class="mt-3 d-flex align-items-center justify-content-between flex-wrap" style="gap:6px;" aria-label="Paginación de expedientes">
      <small class="text-muted">Pág. ${page} de ${pages} &mdash; ${total} resultados</small>
      <ul class="pagination pagination-sm mb-0">
        <li class="page-item ${page <= 1 ? 'disabled' : ''}">
          <button class="page-link" data-rrhh-page="1" aria-label="Primera página"><i class="fas fa-angle-double-left" aria-hidden="true"></i></button>
        </li>
        <li class="page-item ${page <= 1 ? 'disabled' : ''}">
          <button class="page-link" data-rrhh-page="${page - 1}" aria-label="Página anterior"><i class="fas fa-chevron-left" aria-hidden="true"></i></button>
        </li>
        ${pageNums.map(p => `<li class="page-item ${p === page ? 'active' : ''}">
          <button class="page-link" data-rrhh-page="${p}" aria-label="Página ${p}" ${p === page ? 'aria-current="page"' : ''}>${p}</button>
        </li>`).join("")}
        <li class="page-item ${page >= pages ? 'disabled' : ''}">
          <button class="page-link" data-rrhh-page="${page + 1}" aria-label="Página siguiente"><i class="fas fa-chevron-right" aria-hidden="true"></i></button>
        </li>
        <li class="page-item ${page >= pages ? 'disabled' : ''}">
          <button class="page-link" data-rrhh-page="${pages}" aria-label="Última página"><i class="fas fa-angle-double-right" aria-hidden="true"></i></button>
        </li>
      </ul>
    </nav>`;
  container.querySelectorAll("[data-rrhh-page]").forEach(btn => {
    btn.addEventListener("click", () => changeRrhhPage(Number(btn.dataset.rrhhPage)));
  });
}

function changeRrhhPage(p) {
  state.rrhh.page = p;
  triggerRrhhSearch();
  // Devuelve el scroll y el foco al principio de la lista (BR-027)
  const header = document.querySelector(".ds-results-header");
  if (header) { header.setAttribute("tabindex", "-1"); header.scrollIntoView({ behavior: "smooth", block: "start" }); header.focus({ preventScroll: true }); }
}

// El usuario puede editar RRHH sólo si es Admin de ese módulo (BR-024)
function _esAdminRrhh() {
  return !!(state.user && state.user.roles && state.user.roles["RRHH"] === "Admin");
}

function renderRrhhList() {
  const container = document.getElementById("list_rrhh");
  const results   = state.rrhh.results;
  const total     = state.rrhh.total || results.length;
  const hasFilter = !!(state.rrhh.search || (state.rrhh.selectedTypes && state.rrhh.selectedTypes.length) || (state.rrhh.selectedEstados && state.rrhh.selectedEstados.length));
  document.getElementById("count-rrhh-results").innerText = hasFilter ? `${total} Resultados` : `${total} Registros`;

  if (results.length === 0) {
    container.removeAttribute("role");
    const emptyMsg = hasFilter
      ? `No se encontraron expedientes con los filtros aplicados.
         <br><small class="text-muted">Intente ampliar la búsqueda o limpiar los filtros.</small>`
      : `No hay expedientes registrados en el sistema.`;
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-users fa-2x mb-2 text-muted" aria-hidden="true"></i>
      <p class="mb-0">${emptyMsg}</p>
      ${hasFilter ? `<button type="button" class="btn btn-sm btn-outline-secondary mt-2" data-rrhh-clear-empty">Limpiar filtros</button>` : ""}
    </div>`;
    const clearBtn = container.querySelector("[data-rrhh-clear-empty]");
    if (clearBtn) clearBtn.addEventListener("click", () => document.getElementById("btn_clear_rrhh")?.click());
    document.getElementById("info-rrhh-pagination").innerText = "Pág 1 de 1";
    const prevBtn = document.getElementById("btn-rrhh-prev");
    const nextBtn = document.getElementById("btn-rrhh-next");
    if (prevBtn) prevBtn.disabled = true;
    if (nextBtn) nextBtn.disabled = true;
    return;
  }

  container.setAttribute("role", "list");
  const totalPages = Math.ceil(total / state.rrhh.perPage) || 1;
  document.getElementById("info-rrhh-pagination").innerText = `Pág ${state.rrhh.page} de ${totalPages}`;
  const prevBtnR = document.getElementById("btn-rrhh-prev");
  const nextBtnR = document.getElementById("btn-rrhh-next");
  if (prevBtnR) prevBtnR.disabled = state.rrhh.page <= 1;
  if (nextBtnR) nextBtnR.disabled = state.rrhh.page >= totalPages;

  const searchTerms = (state.rrhh.search || "").trim().split(/\s+/).filter(t => t.length > 1);
  const hl = txt => typeof highlightTerms === "function" ? highlightTerms(txt, searchTerms) : (txt || "");
  const puedeEditar = _esAdminRrhh();

  container.innerHTML = results.map(p => {
    const initials   = getPersonInitials(p.persona_raw);
    const colorState = getStatusColor(p.estatuses);
    const nombreEsc  = escHtml(p.persona || "el empleado");
    const tipos = (p.tipos || "").split(";").filter(Boolean);
    const tiposVisibles = tipos.slice(0, 3);
    const tiposRestantes = tipos.length - tiposVisibles.length;
    return `
      <div class="ds-item-card ds-person-card" role="listitem" style="border-left:3px solid ${colorState};">
        <button type="button" class="ds-person-card-open" data-rrhh-open="${escHtml(p.persona_raw || "")}"
          aria-label="Ver expediente de ${nombreEsc}"
          style="all:unset;cursor:pointer;display:flex;align-items:center;flex:1 1 auto;min-width:0;">
          <div class="ds-item-thumbnail" style="align-items:center;padding-top:0;">
            <div style="width:54px;height:54px;border-radius:50%;overflow:hidden;display:flex;align-items:center;justify-content:center;background:#eef4fb;border:2px solid ${colorState};flex-shrink:0;">
              ${p.foto_url
                ? `<img src="${_secureFileUrl(p.foto_url)}" alt="Fotografía de ${nombreEsc}" loading="lazy" decoding="async" data-rrhh-initials="${escHtml(initials)}" style="width:100%;height:100%;object-fit:cover;display:block;">`
                : `<span style="width:100%;height:100%;background:#2b4e72;color:#fff;display:flex;align-items:center;justify-content:center;font-weight:800;font-size:0.9rem;">${initials}</span>`}
            </div>
          </div>
          <div class="ds-item-metadata" style="flex-grow:1;padding-left:15px;min-width:0;text-align:left;">
            <h3 class="ds-item-title" style="font-size:1.05rem;font-weight:700;color:#2b4e72;margin:0 0 3px 0;">${hl(p.persona)}</h3>
            <div style="font-size:0.82rem;color:#495057;line-height:1.5;">
              <span class="mr-3"><i class="fas fa-id-card mr-1 text-muted" aria-hidden="true"></i> C.I: <strong>${hl(p.cedulas)}</strong></span>
              <span class="mr-3"><i class="fas fa-sitemap mr-1 text-muted" aria-hidden="true"></i> <strong>${hl(p.departamentos)}</strong></span>
              <span><i class="fas fa-user-tie mr-1 text-muted" aria-hidden="true"></i> <strong>${hl(p.cargos)}</strong></span>
            </div>
            <div class="mt-2 d-flex align-items-center flex-wrap" style="gap:4px;">
              <span class="badge" style="background-color:${colorState};color:white;padding:3px 8px;border-radius:10px;font-size:0.78rem;font-weight:700;">${escHtml(p.estatuses)}</span>
              <span class="badge badge-light border" style="padding:2px 7px;border-radius:10px;font-size:0.7rem;"><i class="fas fa-file-alt mr-1" aria-hidden="true"></i>${Number(p.doc_count) || 0} docs</span>
              ${tiposVisibles.map(t => `<span class="badge badge-secondary" style="padding:2px 6px;border-radius:8px;font-size:0.68rem;max-width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;display:inline-block;" title="${escHtml(t.trim())}">${escHtml(_truncBadgeRrhh(t.trim()))}</span>`).join("")}
              ${tiposRestantes > 0 ? `<span class="badge badge-secondary" title="${escHtml(tipos.slice(3).join(", "))}" style="padding:2px 6px;border-radius:8px;font-size:0.68rem;">+${tiposRestantes}</span>` : ""}
            </div>
          </div>
        </button>
        <div class="ds-item-actions" style="margin-left:12px;display:flex;flex-direction:column;justify-content:center;gap:6px;">
          <button class="btn btn-primary ds-action-btn" aria-label="Ver expediente de ${nombreEsc}" title="Ver expediente"
            data-rrhh-open="${escHtml(p.persona_raw || "")}"
            style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
            <i class="fas fa-eye" aria-hidden="true"></i>
          </button>
          ${puedeEditar ? `<a href="/static/admin_hr.html?empId=${encodeURIComponent(p.empleado_id)}" class="btn btn-outline-warning ds-action-btn" aria-label="Editar expediente de ${nombreEsc}" title="Editar expediente (Admin)"
            style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
            <i class="fas fa-pen" style="font-size:0.8rem;" aria-hidden="true"></i>
          </a>` : ""}
        </div>
      </div>
    `;
  }).join("");
  container.querySelectorAll("[data-rrhh-open]").forEach(el => {
    el.addEventListener("click", (e) => { e.stopPropagation(); openRrhhPersonDossier(el.dataset.rrhhOpen, el); });
  });
  // Fallback a iniciales si la foto no carga (BR-037)
  container.querySelectorAll("img[data-rrhh-initials]").forEach(img => {
    img.addEventListener("error", () => {
      const span = document.createElement("span");
      span.style.cssText = "width:100%;height:100%;background:#2b4e72;color:#fff;display:flex;align-items:center;justify-content:center;font-weight:800;font-size:0.9rem;";
      span.textContent = img.dataset.rrhhInitials || "";
      img.replaceWith(span);
    }, { once: true });
  });
}

// ==========================================================================
// DOSSIER DE PERSONA
// ==========================================================================
// Elemento que abrió el dossier, para devolverle el foco al cerrar (BR-080)
let _rrhhDossierTrigger = null;

async function openRrhhPersonDossier(personaRaw, triggerEl) {
  _rrhhDossierTrigger = triggerEl || document.activeElement;
  const modalContent = document.getElementById("rrhh-person-modal-content");
  if (modalContent) {
    modalContent.innerHTML = `<div class="text-center p-5"><span class="spinner-border text-primary mb-2" role="status"></span><p class="text-muted mb-0">Cargando expediente…</p></div>`;
  }
  $("#rrhh-person-modal").modal("show");
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/person/profile`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ persona: personaRaw })
    });
    if (!res.ok) throw new Error(String(res.status));
    state.activePersonProfile = await res.json();
    state.innerDossierSearch  = "";
    state.innerDossierClass   = "";
    state.innerDossierSort    = "Alfabético (A-Z)";
    renderRrhhDossierModal();
  } catch (e) {
    console.error("Error al abrir perfil de RRHH:", e);
    $("#rrhh-person-modal").modal("hide");
    showToast("No se pudo abrir el expediente. Intente de nuevo.", "error");
  }
}

// Normaliza texto: minúsculas y sin acentos (para comparar tipos de documento)
function normalizeDocText(text) {
  return (text || "")
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "")
    .trim();
}

// Busca en el expediente el primer documento cuyo doc_type o título coincida con alguna palabra clave
function findPersonKeyDoc(profile, keywords) {
  if (!profile?.rows) return null;
  for (const row of profile.rows) {
    const docType = normalizeDocText(row.doc_type);
    const titulo  = normalizeDocText(row.titulo_doc);
    if (keywords.some(k => docType === k || docType.includes(k) || titulo.includes(k))) {
      return row;
    }
  }
  return null;
}

// Renderiza los botones de acceso directo a documentos de identidad del expediente
function renderQuickDocLinks(profile) {
  const quickDocs = [
    { label: "Cédula",            icon: "fa-id-card",        keywords: ["cedula"] },
    { label: "RIF",               icon: "fa-file-invoice",   keywords: ["rif", "registro de informacion fiscal"] },
    { label: "Currículo Vitae",   icon: "fa-file-alt",       keywords: ["cv", "curriculum"] },
    { label: "Planilla de Datos", icon: "fa-clipboard-list", keywords: ["actualizacion de datos", "datos personales", "planilla de datos"] },
  ];

  return quickDocs.map(qd => {
    const doc = findPersonKeyDoc(profile, qd.keywords);
    if (doc) {
      return `
        <button type="button" class="btn btn-sm btn-outline-primary mr-2 mb-2"
          data-open-doc-idx="${escHtml(String(doc.__idx))}" title="Ver ${escHtml(qd.label)} en el expediente">
          <i class="fas ${qd.icon} mr-1" aria-hidden="true"></i>${escHtml(qd.label)}
        </button>`;
    }
    return `
      <button type="button" class="btn btn-sm btn-outline-secondary mr-2 mb-2" aria-disabled="true"
        title="${qd.label} no registrado en el expediente">
        <i class="fas ${qd.icon} mr-1" aria-hidden="true"></i>${qd.label} <small>(no registrado)</small>
      </button>`;
  }).join("");
}

// Calcula la edad a partir de los componentes de la fecha, sin pasar por la
// zona horaria del navegador (BR-035): "1980-05-14" no debe leerse como
// medianoche UTC, que en Venezuela cae un día antes.
function _calcEdad(fechaNac) {
  if (!fechaNac) return null;
  const m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(fechaNac));
  if (!m) return null;
  const [, y, mo, d] = m.map(Number);
  const hoy = new Date();
  let edad = hoy.getFullYear() - y;
  const mesesDiff = (hoy.getMonth() + 1) - mo;
  if (mesesDiff < 0 || (mesesDiff === 0 && hoy.getDate() < d)) edad--;
  return edad > 0 ? edad : null;
}

function renderRrhhDossierModal() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  const initials  = getPersonInitials(profile.persona_raw);
  const photoHtml = profile.foto_url
    ? `<div class="rrhh-person-photo-card"><img src="${_secureFileUrl(profile.foto_url)}" class="rrhh-person-photo" alt="${escHtml(profile.persona)}"></div>`
    : `<div class="rrhh-person-photo-card rrhh-person-photo-fallback"><span class="rrhh-person-photo-initials">${escHtml(initials)}</span><i class="fas fa-user rrhh-person-photo-icon" aria-hidden="true"></i></div>`;

  const isRetirado  = (profile.statuses || "").includes("Retirado");
  const isPensionado = (profile.statuses || "").includes("Pensionado");
  const cedulaDoc = findPersonKeyDoc(profile, ["cedula"]);
  const ciHtml = profile.cedulas
    ? `${escHtml(profile.cedulas)}${cedulaDoc ? ` <button type="button" class="btn btn-xs btn-outline-primary ml-2 py-0 px-2" data-open-doc-idx="${escHtml(String(cedulaDoc.__idx))}"><i class="fas fa-id-card" aria-hidden="true"></i> Ver</button>` : ""}`
    : "N/A";
  const empleadoId = profile.rows && profile.rows[0]?.empleado_id;

  // Derivar categorías visibles desde los documentos (usa slug→parte canónica como fallback)
  const docCatSet = new Set();
  for (const row of (profile.rows || [])) {
    if (row.categoria_slug) {
      const p = RRHH_PARTES.find(x => x.slug === row.categoria_slug);
      docCatSet.add(p ? p.nombre : (row.categoria || row.doc_type || "Sin clasificar"));
    } else if (row.categoria) {
      docCatSet.add(row.categoria);
    } else if (row.doc_type) {
      docCatSet.add(row.doc_type);
    }
  }
  const docTypes = [...docCatSet].sort((a, b) => {
    const po = Object.fromEntries(RRHH_PARTES.map((p, i) => [p.nombre, i]));
    return (po[a] ?? 99) - (po[b] ?? 99) || a.localeCompare(b, "es", { sensitivity: "base", numeric: true });
  });
  document.getElementById("rrhh-person-modal-content").innerHTML = `
    <div class="ds-person-profile-header mb-4 p-3 bg-white rounded shadow-sm border">
      <div class="d-flex flex-column flex-md-row align-items-center align-items-md-start">
        <div class="ds-person-avatar-wrap ds-dossier-avatar-wrap mb-3 mb-md-0 mr-md-4">
          ${photoHtml}
        </div>
        <div class="ds-person-info flex-grow-1 w-100">
          <div class="d-flex justify-content-between align-items-center border-bottom pb-2 mb-3">
            <h3 id="rrhh-person-modal-title" class="ds-person-name m-0 text-primary font-weight-bold">${escHtml(profile.persona)}</h3>
            <div class="d-flex align-items-center">
              <span class="badge badge-info text-uppercase px-3 py-2">${escHtml(profile.statuses || "Sin estado")}</span>
              ${state.user && empleadoId
                ? `<a href="${API_BASE}/api/rrhh/report/${empleadoId}" target="_blank" rel="noopener noreferrer" class="btn btn-outline-secondary btn-sm ml-2" title="Generar reporte imprimible">
                    <i class="fas fa-print mr-1" aria-hidden="true"></i>Imprimir Expediente
                  </a>`
                : ''}
              ${_esAdminRrhh() && empleadoId
                ? `<a href="/static/admin_hr.html?empId=${encodeURIComponent(empleadoId)}" class="btn btn-outline-warning btn-sm ml-2" title="Editar expediente en panel de administración">
                    <i class="fas fa-pen mr-1" aria-hidden="true"></i>Editar
                  </a>`
                : ''}
            </div>
          </div>
          <p class="ds-person-cargo text-secondary mb-3 font-weight-bold">
            <i class="fas fa-user-tie mr-2" aria-hidden="true"></i>${escHtml(profile.cargos || "Cargo no especificado")}
          </p>
          <div class="row">
            <div class="col-6 mb-2"><strong>C.I.:</strong> ${ciHtml}</div>
            ${profile.rifs ? `<div class="col-6 mb-2"><strong>RIF:</strong> ${escHtml(profile.rifs)}</div>` : ""}
            ${profile.departamentos ? `<div class="col-6 mb-2"><strong>Adscripción:</strong> ${escHtml(profile.departamentos)}</div>` : ""}
            <div class="col-6 mb-2"><strong>Ingreso:</strong> ${escHtml(formatISOToSpanish(profile.fecha_ingreso) || "No registrada")}</div>
            ${profile.fecha_nacimiento ? `<div class="col-6 mb-2"><strong>Nacimiento:</strong> ${escHtml(formatISOToSpanish(profile.fecha_nacimiento))}${_calcEdad(profile.fecha_nacimiento) ? ` <span class="text-muted small">(${_calcEdad(profile.fecha_nacimiento)} años)</span>` : ""}</div>` : ""}
            ${profile.nivel_educativo  ? `<div class="col-6 mb-2"><strong>Nivel Educ.:</strong> <span class="badge badge-light border">${escHtml(profile.nivel_educativo)}</span></div>` : ""}
            ${profile.sexo             ? `<div class="col-6 mb-2"><strong>Sexo:</strong> ${escHtml({ M: "Masculino", F: "Femenino", O: "Otro" }[profile.sexo] || profile.sexo)}</div>` : ""}
            ${profile.fecha_jubilacion ? `<div class="col-6 mb-2"><strong>Jubilación${isRetirado ? " (efectiva)" : " (prevista)"}:</strong> ${escHtml(formatISOToSpanish(profile.fecha_jubilacion))}</div>` : ""}
            ${isPensionado && profile.fecha_pension ? `<div class="col-6 mb-2"><strong>Pensión:</strong> ${escHtml(formatISOToSpanish(profile.fecha_pension))}</div>` : ""}
          </div>
          <div class="border-top pt-3 mt-2">
            <h4 class="font-weight-bold text-secondary text-uppercase mb-2 ds-dossier-subheading">
              <i class="fas fa-folder mr-2" aria-hidden="true"></i>Documentos de Identidad
            </h4>
            <div class="d-flex flex-wrap">
              ${renderQuickDocLinks(profile)}
            </div>
          </div>
          <div class="border-top pt-3 mt-2">
            <div class="d-flex justify-content-between align-items-center mb-2">
              <h4 class="font-weight-bold text-secondary text-uppercase mb-0 ds-dossier-subheading">
                <i class="fas fa-briefcase mr-2" aria-hidden="true"></i>Historial de Cargos
              </h4>
              <button type="button" class="btn btn-xs btn-outline-secondary" ${empleadoId ? "" : 'aria-disabled="true"'}
                data-toggle-historial="${escHtml(String(empleadoId || ""))}"
                title="${empleadoId ? "" : "Sin identificador de empleado disponible"}">
                <i class="fas fa-history mr-1" aria-hidden="true"></i>Ver historial
              </button>
            </div>
            <div id="historial-cargos-inline" class="bg-light rounded p-2 ds-dossier-historial-inline d-none"></div>
          </div>
        </div>
      </div>
    </div>
    <div class="ds-modal-filters-wrap bg-light p-3 rounded border mb-4">
      <h4 class="font-weight-bold text-secondary text-uppercase mb-3">
        <i class="fas fa-sliders-h mr-2" aria-hidden="true"></i>Explorar Documentos
      </h4>
      <div class="row">
        <div class="col-md-5 mb-2">
          <input type="text" id="inner-dossier-search" class="form-control form-control-sm"
            placeholder="Buscar palabras, ubicaciones, fechas..."
            oninput="state.innerDossierSearch=this.value;filterInnerDossier();">
        </div>
        <div class="col-md-4 mb-2">
          <select id="inner-dossier-class" class="form-control form-control-sm"
            onchange="state.innerDossierClass=this.value;filterInnerDossier();">
            <option value="">Todas las categorías</option>
            ${docTypes.map(c => `<option value="${escHtml(c)}">${escHtml(c)}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-3 mb-2">
          <select id="inner-dossier-sort" class="form-control form-control-sm"
            onchange="state.innerDossierSort=this.value;filterInnerDossier();">
            <option value="Alfabético (A-Z)">Alfabético (A-Z)</option>
            <option value="Alfabético (Z-A)">Alfabético (Z-A)</option>
            <option value="Más recientes primero">Más recientes primero</option>
            <option value="Más antiguos primero">Más antiguos primero</option>
          </select>
        </div>
      </div>
      <div class="text-right mt-1">
        <span id="inner-dossier-folio-count" class="badge badge-pill badge-primary px-3 py-1">0 folios visibles</span>
      </div>
    </div>
    <div class="rrhh-person-files px-1" id="inner-dossier-items-container"></div>
  `;
  // El contenido se genera de nuevo cada vez que se abre el dossier; el botón
  // no tenía manejador atado en ningún sitio y "Ver historial" no hacía nada.
  const histBtn = document.querySelector("#rrhh-person-modal-content [data-toggle-historial]");
  if (histBtn) histBtn.addEventListener("click", () => _toggleHistorialCargos(empleadoId || null));
  filterInnerDossier();
}

// Orden canónico de las 4 partes del expediente RRHH
const RRHH_PARTES = [
  { slug: "parte-i",   nombre: "Parte I — Ingreso y Contratación",   icon: "fas fa-file-signature", color: "#0d6efd" },
  { slug: "parte-ii",  nombre: "Parte II — Escalafón y Desarrollo",  icon: "fas fa-chart-line",     color: "#198754" },
  { slug: "parte-iii", nombre: "Parte III — Permisos y Formación",   icon: "fas fa-calendar-check", color: "#fd7e14" },
  { slug: "parte-iv",  nombre: "Parte IV — Documentos Personales",   icon: "fas fa-id-card",        color: "#6f42c1" },
];

function _docLabel(f) {
  return f.doc_type || f.titulo_doc || f.notas?.split("\n")[0] || "Documento sin tipo";
}

function filterInnerDossier() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  let files = [...profile.rows];

  if (state.innerDossierSearch) {
    const q = state.innerDossierSearch.toLowerCase().trim();
    files = files.filter(f =>
      (_docLabel(f)).toLowerCase().includes(q) ||
      (f.doc_type || "").toLowerCase().includes(q) ||
      (f.titulo_doc || "").toLowerCase().includes(q) ||
      (f.ubicacion || "").toLowerCase().includes(q) ||
      (f.personas_relacionadas || "").toLowerCase().includes(q) ||
      (f.notas || "").toLowerCase().includes(q)
    );
  }
  const dossierTerms = state.innerDossierSearch
    ? state.innerDossierSearch.trim().split(/\s+/).filter(t => t.length > 1)
    : [];
  if (state.innerDossierClass) {
    files = files.filter(f => (f.categoria || f.doc_type || "") === state.innerDossierClass);
  }

  const sortKey = state.innerDossierSort;
  if      (sortKey === "Alfabético (A-Z)")      files.sort((a, b) => _docLabel(a).localeCompare(_docLabel(b), "es", { sensitivity: "base", numeric: true }));
  else if (sortKey === "Alfabético (Z-A)")      files.sort((a, b) => _docLabel(b).localeCompare(_docLabel(a), "es", { sensitivity: "base", numeric: true }));
  else if (sortKey === "Más recientes primero") files.sort((a, b) => (b.fecha_documento || b.fecha_ingreso || "").localeCompare(a.fecha_documento || a.fecha_ingreso || ""));
  else if (sortKey === "Más antiguos primero")  files.sort((a, b) => (a.fecha_documento || a.fecha_ingreso || "").localeCompare(b.fecha_documento || b.fecha_ingreso || ""));

  const countBadge = document.getElementById("inner-dossier-folio-count");
  // "Folio" es un término técnico de archivo (un documento puede tener varios);
  // lo correcto aquí es contar documentos, no inventar un conteo de folios (BR-164)
  if (countBadge) countBadge.textContent = `${files.length} documentos visibles`;

  const container = document.getElementById("inner-dossier-items-container");
  if (files.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-3">
      No se encontraron archivos con estos filtros en el expediente.
      <br><button type="button" class="btn btn-sm btn-outline-secondary mt-2" data-clear-inner-dossier-filters>Limpiar filtros del expediente</button>
    </div>`;
    const clearBtn = container.querySelector("[data-clear-inner-dossier-filters]");
    if (clearBtn) clearBtn.addEventListener("click", () => {
      state.innerDossierSearch = ""; state.innerDossierClass = "";
      const s = document.getElementById("inner-dossier-search"); if (s) s.value = "";
      const c = document.getElementById("inner-dossier-class");  if (c) c.value = "";
      filterInnerDossier();
    });
    return;
  }

  // Agrupar por parte (categoria_slug → parte canónica) o por categoria/doc_type.
  // Los documentos sin clasificar se separan aparte: son una tarea de archivo
  // pendiente, no una Parte más del expediente (BR-166).
  const grouped = {};
  const sinClasificar = [];
  for (const f of files) {
    let key;
    if (f.categoria_slug) {
      const parte = RRHH_PARTES.find(p => p.slug === f.categoria_slug);
      key = parte ? parte.nombre : (f.categoria || f.doc_type || null);
    } else {
      key = f.categoria || f.doc_type || null;
    }
    if (!key) { sinClasificar.push(f); continue; }
    if (!grouped[key]) grouped[key] = [];
    grouped[key].push(f);
  }

  // Ordenar grupos según el orden canónico de las partes
  const parteOrder = Object.fromEntries(RRHH_PARTES.map((p, i) => [p.nombre, i]));
  const sortedGroups = Object.entries(grouped).sort(([a], [b]) => {
    const oa = parteOrder[a] ?? 99;
    const ob = parteOrder[b] ?? 99;
    return oa !== ob ? oa - ob : a.localeCompare(b, "es", { sensitivity: "base" });
  });
  if (sinClasificar.length) sortedGroups.push(["__sin_clasificar__", sinClasificar]);

  const slugOf = cat => cat === "__sin_clasificar__"
    ? "sin-clasificar"
    : (RRHH_PARTES.find(p => p.nombre === cat)?.slug
        || cat.toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "") || "grupo");

  // Renderizar como tabs si hay múltiples grupos, secciones si solo hay uno
  if (sortedGroups.length > 1) {
    const tabId = "dossier-partes-tabs";
    const tabsHtml = sortedGroups.map(([cat], i) => {
      const esSinClasificar = cat === "__sin_clasificar__";
      const parte = RRHH_PARTES.find(p => p.nombre === cat);
      const color = parte?.color || "#6c757d";
      const icon  = esSinClasificar
        ? `<i class="fas fa-exclamation-circle mr-1 ds-dossier-parte-icon" aria-hidden="true" style="--ds-parte-color:${color}"></i>`
        : (parte ? `<i class="${parte.icon} mr-1 ds-dossier-parte-icon" aria-hidden="true" style="--ds-parte-color:${color}"></i>` : `<i class="fas fa-folder mr-1" aria-hidden="true"></i>`);
      const count = grouped[cat]?.length ?? sinClasificar.length;
      const slug = slugOf(cat);
      const label = esSinClasificar ? "Sin clasificar" : cat.replace(/ — .+/, "");
      return `<li class="nav-item" role="presentation">
        <a class="nav-link${i === 0 ? " active" : ""} ds-dossier-parte-tab" role="tab" id="dossier-tab-btn-${slug}"
          data-toggle="tab" href="#dossier-tab-${slug}" aria-controls="dossier-tab-${slug}" aria-selected="${i === 0 ? "true" : "false"}"
          title="${escHtml(esSinClasificar ? "Documentos sin clasificar — revisar" : cat)}">
          ${icon}${escHtml(label)}
          <span class="badge ml-1 ds-dossier-parte-badge" style="--ds-parte-color:${color}">${count}</span>
        </a>
      </li>`;
    }).join("");

    const panelsHtml = sortedGroups.map(([cat, catFiles], i) => {
      const slug = slugOf(cat);
      const aviso = cat === "__sin_clasificar__"
        ? `<div class="alert alert-warning py-2 px-3 mb-3"><i class="fas fa-exclamation-triangle mr-2" aria-hidden="true"></i>${catFiles.length} documento(s) sin clasificar — pendientes de asignar a una Parte del expediente.</div>`
        : "";
      return `<div class="tab-pane fade${i === 0 ? " show active" : ""}" id="dossier-tab-${slug}" role="tabpanel" aria-labelledby="dossier-tab-btn-${slug}">
        ${aviso}${_renderDossierFileList(catFiles, dossierTerms)}
      </div>`;
    }).join("");

    container.innerHTML = `
      <ul class="nav nav-tabs mb-3" id="${tabId}" role="tablist">${tabsHtml}</ul>
      <div class="tab-content">${panelsHtml}</div>`;
    container.querySelectorAll('[data-toggle="tab"]').forEach(tabEl => {
      tabEl.addEventListener("shown.bs.tab", () => {
        container.querySelectorAll('[data-toggle="tab"]').forEach(t => t.setAttribute("aria-selected", t.classList.contains("active") ? "true" : "false"));
      });
    });
  } else if (sortedGroups.length === 1) {
    // Un solo grupo: sin tabs
    const [, catFiles] = sortedGroups[0];
    container.innerHTML = _renderDossierFileList(catFiles, dossierTerms);
  } else {
    // files.length > 0 pero ningún documento produjo una clave de grupo
    // (guarda explícita para no depender de que _docLabel nunca devuelva
    // clave vacía -- BR-041)
    container.innerHTML = _renderDossierFileList(files, dossierTerms);
  }
  _wireDossierOpenButtons(container);
}

function _renderDossierFileList(catFiles, dossierTerms) {
  return catFiles.map(f => {
    const hlD = txt => typeof highlightTerms === "function" ? highlightTerms(txt, dossierTerms) : (txt || "");
    const label = _docLabel(f);
    return `
    <div class="rrhh-person-file-item">
      <div class="rrhh-person-file-head">
        <div class="rrhh-person-file-main">
          <strong>${hlD(label)}</strong>
          ${f.titulo_doc && f.titulo_doc !== label ? `<span class="text-muted small ml-2">${hlD(f.titulo_doc)}</span>` : ""}
          <span class="rrhh-person-file-sub">Fecha: ${formatISOToSpanish(f.fecha_documento || f.fecha_ingreso)}</span>
        </div>
        <button type="button" class="btn btn-sm btn-outline-info"
          onclick="openDocMetadataModal(${JSON.stringify(f.__idx)})">Abrir archivo</button>
      </div>
      <div class="rrhh-person-file-meta">
        <span>Dependencia: <strong>${escHtml(f.departamento || "N/A")}</strong></span>
        <span>Estatus: <strong>${escHtml(f.estatus || f.estado || "N/A")}</strong></span>
        <span>Ubicación: <strong>${hlD(f.ubicacion) || "N/A"}</strong></span>
        ${f.notas ? `<span>Notas: <em>${hlD(f.notas)}</em></span>` : ""}
      </div>
    </div>`;
  }).join("");
}

function openDocMetadataModal(idxReal) {
  if (!state.activePersonProfile?.rows) return;
  const doc = state.activePersonProfile.rows.find(d => d.__idx == idxReal);
  if (!doc) return;

  const _lbl = _docLabel(doc);
  document.getElementById("modal-doc-title").innerText       = `${_lbl} - ${doc.empleado}`;
  document.getElementById("modal-doc-thumb-icon").className  = "fas fa-id-card";
  document.getElementById("modal-doc-thumb-badge").innerText = _lbl;

  const docLbl = _docLabel(doc);
  document.getElementById("modal-doc-meta-container").innerHTML = `
    <div class="ds-doc-meta-row"><span class="k">Titular</span><span class="v">${escHtml(doc.empleado)}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Cédula</span><span class="v">${escHtml(doc.cedula)}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Tipo de Documento</span><span class="v">${escHtml(docLbl)}</span></div>
    ${doc.titulo_doc && doc.titulo_doc !== docLbl ? `<div class="ds-doc-meta-row"><span class="k">Título</span><span class="v">${escHtml(doc.titulo_doc)}</span></div>` : ""}
    ${doc.categoria ? `<div class="ds-doc-meta-row"><span class="k">Clasificación</span><span class="v">${escHtml(doc.categoria)}</span></div>` : ""}
    <div class="ds-doc-meta-row"><span class="k">Personas vinculadas</span><span class="v">${escHtml(doc.personas_relacionadas || "N/A")}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${escHtml(doc.ubicacion || "N/A")}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Fecha del Documento</span><span class="v">${escHtml(formatISOToSpanish(doc.fecha_documento || doc.fecha_ingreso))}</span></div>
    ${doc.notas ? `<div class="ds-doc-meta-row"><span class="k">Notas</span><span class="v">${escHtml(doc.notas)}</span></div>` : ""}
  `;
  // "Sin descripción registrada" en vez de fabricar una a partir de otros
  // campos del documento -- inventar metadatos descriptivos es un error de
  // fondo en un sistema de archivo, no un detalle de estilo (BR-038).
  document.getElementById("modal-doc-abstract").innerText = doc.notas || "Sin descripción registrada.";

  closeDocViewer();
  const fileUrl = _secureFileUrl(doc.file_url || "");
  const viewBtn = document.getElementById("btn-modal-view");
  // Un botón "Ver" que no muestra el documento es una acción fallida
  // disfrazada de disponible; sin archivo se deshabilita con el motivo en el
  // título en vez de reetiquetarse como otra acción (BR-039). La ubicación
  // física ya se lista en el panel de metadatos.
  if (fileUrl) {
    viewBtn.disabled = false;
    viewBtn.removeAttribute("title");
    viewBtn.innerHTML = '<i class="fas fa-file-pdf mr-1" aria-hidden="true"></i>Ver PDF';
    viewBtn.onclick = () => {
      toggleDocViewer(fileUrl);
      // El <iframe> del visor llevaba siempre el mismo título genérico
      // ("Visor del documento"); con el nombre real un lector de pantalla
      // distingue qué documento se está mostrando (BR-100).
      const iframe = document.getElementById("modal-doc-iframe");
      if (iframe) iframe.title = `Visor del documento: ${_lbl}`;
    };
  } else {
    const motivo = (doc.ubicacion || "").toLowerCase().includes("digitalizado")
      ? "Documento marcado como digitalizado, pero sin archivo asignado (incidencia de datos)."
      : "No hay archivo digital asociado a este documento.";
    viewBtn.disabled = true;
    viewBtn.title = motivo;
    viewBtn.innerHTML = '<i class="fas fa-eye-slash mr-1" aria-hidden="true"></i>Ver';
    viewBtn.onclick = null;
  }

  $("#doc-modal").modal("show");
}

// =============================================================================
// HISTORIAL DE CARGOS — sección del dossier
// =============================================================================

async function _toggleHistorialCargos(empleadoId) {
  const container = document.getElementById("historial-cargos-inline");
  if (!container) return;

  if (!container.classList.contains("d-none")) {
    container.classList.add("d-none");
    return;
  }

  if (!empleadoId) {
    container.innerHTML = '<span class="text-muted">ID de empleado no disponible.</span>';
    container.classList.remove("d-none");
    return;
  }

  container.innerHTML = '<span class="spinner-border spinner-border-sm mr-2 text-secondary"></span>Cargando historial...';
  container.classList.remove("d-none");

  try {
    const res = await fetch(`${API_BASE}/api/rrhh/empleado/${empleadoId}/historial_cargos`);
    // Sin esta comprobación, un 401/500 con cuerpo JSON se leía como
    // "sin movimientos registrados" -- un error de servidor disfrazado de
    // dato de negocio (BR-022).
    if (!res.ok) {
      const msg = res.status === 401 || res.status === 403
        ? "Su sesión expiró o no tiene permiso para ver el historial."
        : "No se pudo cargar el historial de cargos.";
      container.innerHTML = `<span class="text-danger"><i class="fas fa-exclamation-triangle mr-1" aria-hidden="true"></i>${escHtml(msg)}</span>`;
      return;
    }
    const data = await res.json();
    const historial = data.historial || [];

    if (!historial.length) {
      container.innerHTML = '<span class="text-muted"><i class="fas fa-info-circle mr-1" aria-hidden="true"></i>No hay movimientos de cargo registrados.</span>';
      return;
    }

    container.innerHTML = `
      <div class="table-responsive">
        <table class="table table-sm mb-0 ds-dossier-historial-table">
          <thead class="bg-white">
            <tr>
              <th>Cargo</th>
              <th>Desde</th>
              <th>Hasta</th>
              <th>Motivo</th>
            </tr>
          </thead>
          <tbody>
            ${historial.map((h, i) => `
              <tr ${i === 0 ? 'class="font-weight-bold"' : ''}>
                <td>${escHtml(h.cargo || "—")}</td>
                <td>${formatISOToSpanish(h.fecha_inicio) || escHtml(h.fecha_inicio) || "—"}</td>
                <td>${h.fecha_fin ? (formatISOToSpanish(h.fecha_fin) || escHtml(h.fecha_fin)) : '<span class="badge badge-success">Actual</span>'}</td>
                <td class="text-muted">${escHtml(h.motivo || "—")}</td>
              </tr>
            `).join("")}
          </tbody>
        </table>
      </div>
    `;
  } catch {
    container.innerHTML = '<span class="text-danger">Error al cargar el historial de cargos.</span>';
  }
}

// ==========================================================================
// FACETAS RRHH — distribución por departamento y estado
// ==========================================================================
function _renderRrhhFacets(facets) {
  const el = document.getElementById("rrhh-facets-panel");
  if (!el) return;
  if (!facets || (!facets.by_dept?.length && !facets.by_estado?.length)) {
    el.innerHTML = "";
    return;
  }
  const byDept   = (facets.by_dept   || []).slice(0, 10);
  const byEstado = (facets.by_estado || []).slice(0, 6);
  const selectedEstados = state.rrhh.selectedEstados || [];

  const deptRows = byDept.map(f =>
    `<div class="d-flex justify-content-between align-items-center py-1 px-1 rounded ds-facet-row"
          style="cursor:pointer;font-size:0.78rem;" onclick="_facetRrhhDeptClick(${JSON.stringify(f.name)})">
      <span class="text-truncate" style="max-width:140px;" title="${escHtml(f.name)}">${escHtml(f.name)}</span>
      <span class="badge badge-secondary ml-1" style="font-size:0.68rem;min-width:24px;text-align:center;">${f.count}</span>
    </div>`
  ).join("");

  const estadoRows = byEstado.map(f => {
    const active = selectedEstados.includes(f.name);
    return `<div class="d-flex justify-content-between align-items-center py-1 px-1 rounded ds-facet-row${active ? " ds-facet-active" : ""}"
                 style="cursor:pointer;font-size:0.78rem;" onclick="_facetRrhhEstadoClick(${JSON.stringify(f.name)})">
      <span>${escHtml(f.name)}</span>
      <span class="badge badge-secondary ml-1" style="font-size:0.68rem;min-width:24px;text-align:center;">${f.count}</span>
    </div>`;
  }).join("");

  el.innerHTML = `
    <div class="card card-secondary mt-2" style="font-size:0.82rem;">
      <div class="card-header py-1 px-2" style="background:#f4f9ff;">
        <span class="font-weight-bold text-primary" style="font-size:0.8rem;"><i class="fas fa-chart-bar mr-1" aria-hidden="true"></i>Distribución</span>
      </div>
      <div class="card-body p-2">
        ${byDept.length   ? `<p class="text-muted mb-1" style="font-size:0.72rem;text-transform:uppercase;letter-spacing:.04em;">Por Departamento</p>${deptRows}` : ""}
        ${byEstado.length ? `<p class="text-muted mb-1 mt-2" style="font-size:0.72rem;text-transform:uppercase;letter-spacing:.04em;">Por Estado Laboral</p>${estadoRows}` : ""}
      </div>
    </div>`;
}

function _facetRrhhDeptClick(name) {
  // Departamento no es un filtro multi-select en el modelo actual; aplica como búsqueda
  const searchEl = document.getElementById("search_rrhh");
  if (searchEl) searchEl.value = name;
  state.rrhh.search = name;
  state.rrhh.page = 1;
  triggerRrhhSearch();
}

function _facetRrhhEstadoClick(name) {
  if (!state.rrhh.selectedEstados) state.rrhh.selectedEstados = [];
  const idx = state.rrhh.selectedEstados.indexOf(name);
  if (idx >= 0) state.rrhh.selectedEstados.splice(idx, 1);
  else state.rrhh.selectedEstados.push(name);
  state.rrhh.page = 1;
  triggerRrhhSearch();
}

// ==========================================================================
// EVENTOS DE MODAL — limpieza y foco por cualquier vía de cierre
// ==========================================================================
if (typeof $ === "function") {
  $(document).ready(() => {
    // El visor de PDF sólo se cerraba desde el botón "Cerrar" del pie; la X
    // y Escape dejaban el iframe con la URL anterior cargada en el DOM
    // (BR-040).
    $("#doc-modal").on("hidden.bs.modal", () => { if (typeof closeDocViewer === "function") closeDocViewer(); });
    // Bootstrap devuelve el foco al <body> al cerrar el modal porque la
    // tarjeta que lo abrió puede haber sido reemplazada por innerHTML; se
    // devuelve explícitamente al elemento que lo abrió cuando sigue en el
    // DOM (BR-080).
    $("#rrhh-person-modal").on("hidden.bs.modal", () => {
      if (_rrhhDossierTrigger && document.contains(_rrhhDossierTrigger)) _rrhhDossierTrigger.focus();
      _rrhhDossierTrigger = null;
    });
  });
}
