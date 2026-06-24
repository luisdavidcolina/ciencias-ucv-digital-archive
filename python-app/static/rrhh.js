// ==========================================================================
// BÚSQUEDA Y RENDER — RRHH
// ==========================================================================
async function triggerRrhhSearch() {
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        search_term:  state.rrhh.search,
        doc_types:    state.rrhh.selectedTypes,
        estados:      state.rrhh.selectedEstados,
        people_terms: state.rrhh.selectedPeople,
        date_start:   state.rrhh.dateStart,
        date_end:     state.rrhh.dateEnd,
        sort_mode:    state.rrhh.sortMode,
        page:         state.rrhh.page,
        per_page:     state.rrhh.perPage
      })
    });
    if (!res.ok) throw new Error();
    const data = await res.json();
    // Retrocompatibilidad: si la respuesta es un array (formato viejo), tratar como antes
    if (Array.isArray(data)) {
      state.rrhh.results = data;
      state.rrhh.total   = data.length;
    } else {
      state.rrhh.results = data.records || [];
      state.rrhh.total   = data.total   || state.rrhh.results.length;
    }
    renderRrhhList();
    renderRrhhPagination();
  } catch (e) {
    console.error("Error buscando RRHH:", e);
  }
}

function renderRrhhPagination() {
  const container = document.getElementById("rrhh-pagination");
  if (!container) return;
  const total   = state.rrhh.total || state.rrhh.results.length;
  const perPage = state.rrhh.perPage || 10;
  const page    = state.rrhh.page    || 1;
  const pages   = Math.ceil(total / perPage) || 1;
  if (pages <= 1) { container.innerHTML = ""; return; }
  container.innerHTML = `
    <nav class="mt-3 d-flex align-items-center justify-content-between">
      <small class="text-muted">Mostrando pág. ${page} de ${pages} (${total} resultados)</small>
      <ul class="pagination pagination-sm mb-0">
        <li class="page-item ${page <= 1 ? 'disabled' : ''}">
          <button class="page-link" onclick="changeRrhhPage(${page - 1})"><i class="fas fa-chevron-left"></i></button>
        </li>
        ${Array.from({length: Math.min(5, pages)}, (_, i) => {
          const p = Math.max(1, Math.min(page - 2, pages - 4)) + i;
          return `<li class="page-item ${p === page ? 'active' : ''}">
            <button class="page-link" onclick="changeRrhhPage(${p})">${p}</button>
          </li>`;
        }).join("")}
        <li class="page-item ${page >= pages ? 'disabled' : ''}">
          <button class="page-link" onclick="changeRrhhPage(${page + 1})"><i class="fas fa-chevron-right"></i></button>
        </li>
      </ul>
    </nav>`;
}

function changeRrhhPage(p) {
  state.rrhh.page = p;
  triggerRrhhSearch();
}

function renderRrhhList() {
  const container = document.getElementById("list_rrhh");
  const results   = state.rrhh.results;
  const total     = state.rrhh.total || results.length;
  document.getElementById("count-rrhh-results").innerText = `${total} Registros`;

  if (results.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-users fa-2x mb-2 text-muted"></i>
      <p class="mb-0">No se encontraron expedientes coincidentes con los filtros seleccionados.</p>
    </div>`;
    document.getElementById("info-rrhh-pagination").innerText = "Pág 1 de 1";
    const prevBtn = document.getElementById("btn-rrhh-prev");
    const nextBtn = document.getElementById("btn-rrhh-next");
    if (prevBtn) prevBtn.disabled = true;
    if (nextBtn) nextBtn.disabled = true;
    return;
  }

  const totalPages = Math.ceil(total / state.rrhh.perPage) || 1;
  document.getElementById("info-rrhh-pagination").innerText = `Pág ${state.rrhh.page} de ${totalPages}`;
  const prevBtnR = document.getElementById("btn-rrhh-prev");
  const nextBtnR = document.getElementById("btn-rrhh-next");
  if (prevBtnR) prevBtnR.disabled = state.rrhh.page <= 1;
  if (nextBtnR) nextBtnR.disabled = state.rrhh.page >= totalPages;

  container.innerHTML = results.map(p => {
    const initials   = getPersonInitials(p.persona_raw);
    const colorState = getStatusColor(p.estatuses);
    return `
      <div class="ds-item-card ds-person-card" onclick="openRrhhPersonDossier('${p.persona_raw}')" style="cursor:pointer;">
        <div class="ds-item-thumbnail" style="align-items:center;padding-top:0;">
          <div style="width:54px;height:54px;border-radius:50%;overflow:hidden;display:flex;align-items:center;justify-content:center;background:#eef4fb;border:2px solid #dee2e6;flex-shrink:0;">
            ${p.foto_url
              ? `<img src="${p.foto_url}" style="width:100%;height:100%;object-fit:cover;display:block;">`
              : `<span style="width:100%;height:100%;background:#2b4e72;color:#fff;display:flex;align-items:center;justify-content:center;font-weight:800;font-size:0.9rem;">${initials}</span>`}
          </div>
        </div>
        <div class="ds-item-metadata" style="flex-grow:1;padding-left:15px;">
          <h4 class="ds-item-title" style="font-size:1.1rem;font-weight:700;color:#2b4e72;margin:0 0 4px 0;">${p.persona}</h4>
          <div style="font-size:0.82rem;color:#495057;line-height:1.4;">
            <span class="mr-3"><i class="fas fa-id-card mr-1"></i> C.I: <strong>${p.cedulas}</strong></span>
            <span class="mr-3"><i class="fas fa-sitemap mr-1"></i> Adscripción: <strong>${p.departamentos}</strong></span><br>
            <span><i class="fas fa-user-tie mr-1"></i> Cargo: <strong>${p.cargos}</strong></span>
          </div>
          <div class="mt-2 d-flex align-items-center gap-1">
            <span class="badge" style="background-color:${colorState};color:white;padding:4px 8px;border-radius:4px;font-size:0.75rem;">${p.estatuses}</span>
            <span class="badge badge-secondary ml-1" style="padding:4px 8px;border-radius:4px;font-size:0.75rem;">${p.doc_count} documentos en expediente</span>
          </div>
        </div>
        <div class="ds-item-actions" style="margin-left:15px;display:flex;flex-direction:column;justify-content:center;">
          <button class="btn btn-primary ds-action-btn" title="Ver"
            onclick="event.stopPropagation();openRrhhPersonDossier('${p.persona_raw}')"
            style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
            <i class="fas fa-eye"></i>
          </button>
        </div>
      </div>
    `;
  }).join("");
}

// ==========================================================================
// DOSSIER DE PERSONA
// ==========================================================================
async function openRrhhPersonDossier(personaRaw) {
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/person/profile`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ persona: personaRaw })
    });
    if (!res.ok) throw new Error();
    state.activePersonProfile = await res.json();
    state.innerDossierSearch  = "";
    state.innerDossierClass   = "";
    state.innerDossierSort    = "Alfabético (A-Z)";
    renderRrhhDossierModal();
    $("#rrhh-person-modal").modal("show");
  } catch (e) {
    console.error("Error al abrir perfil de RRHH:", e);
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
        <button class="btn btn-sm btn-outline-primary mr-2 mb-2"
          onclick="openDocMetadataModal('${doc.__idx}')" title="Ver ${qd.label} en el expediente">
          <i class="fas ${qd.icon} mr-1"></i>${qd.label}
        </button>`;
    }
    return `
      <button class="btn btn-sm btn-outline-secondary mr-2 mb-2" disabled
        title="${qd.label} no registrado en el expediente">
        <i class="fas ${qd.icon} mr-1"></i>${qd.label} <small>(no registrado)</small>
      </button>`;
  }).join("");
}

function renderRrhhDossierModal() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  const initials  = getPersonInitials(profile.persona_raw);
  const photoHtml = profile.foto_url
    ? `<div class="rrhh-person-photo-card"><img src="${profile.foto_url}" class="rrhh-person-photo" alt="${profile.persona}"></div>`
    : `<div class="rrhh-person-photo-card rrhh-person-photo-fallback"><span class="rrhh-person-photo-initials">${initials}</span><i class="fas fa-user rrhh-person-photo-icon"></i></div>`;

  const isRetirado  = (profile.statuses || "").includes("Retirado");
  const isPensionado = (profile.statuses || "").includes("Pensionado");
  const cedulaDoc = findPersonKeyDoc(profile, ["cedula"]);
  const ciHtml = profile.cedulas
    ? `${profile.cedulas}${cedulaDoc ? ` <a href="#" class="btn btn-xs btn-outline-primary ml-2 py-0 px-2" onclick="openDocMetadataModal('${cedulaDoc.__idx}');return false;"><i class="fas fa-id-card"></i> Ver</a>` : ""}`
    : "N/A";

  const docTypes = profile.categories || [];
  document.getElementById("rrhh-person-modal-content").innerHTML = `
    <div class="ds-person-profile-header mb-4 p-3 bg-white rounded shadow-sm border">
      <div class="d-flex flex-column flex-md-row align-items-center align-items-md-start">
        <div class="ds-person-avatar-wrap mb-3 mb-md-0 mr-md-4" style="width:150px;min-width:150px;">
          ${photoHtml}
        </div>
        <div class="ds-person-info flex-grow-1 w-100">
          <div class="d-flex justify-content-between align-items-center border-bottom pb-2 mb-3">
            <h3 class="ds-person-name m-0 text-primary font-weight-bold">${profile.persona}</h3>
            <div class="d-flex align-items-center">
              <span class="badge badge-info text-uppercase px-3 py-2">${profile.statuses || "Sin estado"}</span>
              ${profile.rows && profile.rows[0]?.empleado_id
                ? `<a href="${API_BASE}/api/rrhh/report/${profile.rows[0].empleado_id}" target="_blank" class="btn btn-outline-secondary btn-sm ml-2" title="Generar reporte imprimible">
                    <i class="fas fa-print mr-1"></i>Imprimir Expediente
                  </a>`
                : ''}
            </div>
          </div>
          <h5 class="ds-person-cargo text-secondary mb-3 font-weight-bold">
            <i class="fas fa-user-tie mr-2"></i>${profile.cargos || "Cargo no especificado"}
          </h5>
          <div class="row">
            <div class="col-6 mb-2"><strong>C.I.:</strong> ${ciHtml}</div>
            <div class="col-6 mb-2"><strong>RIF:</strong> ${profile.rifs || "N/A"}</div>
            <div class="col-6 mb-2"><strong>Adscripción:</strong> ${profile.departamentos || "N/A"}</div>
            <div class="col-6 mb-2"><strong>Ingreso:</strong> ${formatISOToSpanish(profile.fecha_ingreso) || "No registrada"}</div>
            ${isRetirado   ? `<div class="col-6 mb-2"><strong>Jubilación:</strong> ${formatISOToSpanish(profile.fecha_jubilacion) || "No registrada"}</div>` : ""}
            ${isPensionado ? `<div class="col-6 mb-2"><strong>Pensión:</strong>    ${formatISOToSpanish(profile.fecha_pension)    || "No registrada"}</div>` : ""}
          </div>
          <div class="border-top pt-3 mt-2">
            <h6 class="font-weight-bold text-secondary text-uppercase mb-2" style="font-size:0.78rem;">
              <i class="fas fa-folder mr-2"></i>Documentos de Identidad
            </h6>
            <div class="d-flex flex-wrap">
              ${renderQuickDocLinks(profile)}
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="ds-modal-filters-wrap bg-light p-3 rounded border mb-4">
      <h6 class="font-weight-bold text-secondary text-uppercase mb-3">
        <i class="fas fa-sliders-h mr-2"></i>Explorar Documentos
      </h6>
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
            ${docTypes.map(c => `<option value="${c}">${c}</option>`).join("")}
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
  filterInnerDossier();
}

function filterInnerDossier() {
  const profile = state.activePersonProfile;
  if (!profile) return;

  let files = [...profile.rows];

  if (state.innerDossierSearch) {
    const q = state.innerDossierSearch.toLowerCase().trim();
    files = files.filter(f =>
      (f.doc_type || "").toLowerCase().includes(q) ||
      (f.ubicacion || "").toLowerCase().includes(q) ||
      (f.personas_relacionadas || "").toLowerCase().includes(q)
    );
  }
  if (state.innerDossierClass) {
    files = files.filter(f => f.doc_type === state.innerDossierClass);
  }

  const sortKey = state.innerDossierSort;
  if      (sortKey === "Alfabético (A-Z)")      files.sort((a, b) => (a.doc_type || "").localeCompare(b.doc_type || ""));
  else if (sortKey === "Alfabético (Z-A)")      files.sort((a, b) => (b.doc_type || "").localeCompare(a.doc_type || ""));
  else if (sortKey === "Más recientes primero") files.sort((a, b) => (b.fecha_ingreso || "").localeCompare(a.fecha_ingreso || ""));
  else if (sortKey === "Más antiguos primero")  files.sort((a, b) => (a.fecha_ingreso || "").localeCompare(b.fecha_ingreso || ""));

  const countBadge = document.getElementById("inner-dossier-folio-count");
  if (countBadge) countBadge.textContent = `${files.length} folios visibles`;

  const container = document.getElementById("inner-dossier-items-container");
  if (files.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-3">No se encontraron archivos con estos filtros en el expediente.</div>`;
    return;
  }

  // Agrupar por categoría
  const grouped = {};
  for (const f of files) {
    const cat = f.categoria || f.doc_type || "Sin categoría";
    if (!grouped[cat]) grouped[cat] = [];
    grouped[cat].push(f);
  }

  container.innerHTML = Object.entries(grouped).map(([cat, catFiles]) => `
    <div class="rrhh-person-category-section mb-4">
      <h5 class="border-bottom pb-2 mb-3 ds-category-title">
        <i class="fas fa-folder-open mr-2"></i>${cat}
      </h5>
      ${catFiles.map(f => `
        <div class="rrhh-person-file-item">
          <div class="rrhh-person-file-head">
            <div class="rrhh-person-file-main">
              <strong>${f.doc_type}</strong>
              <span class="rrhh-person-file-sub">Fecha de ingreso: ${formatISOToSpanish(f.fecha_ingreso)}</span>
            </div>
            <a href="#" class="btn btn-sm btn-outline-info"
              onclick="openDocMetadataModal('${f.__idx}');return false;">Abrir archivo</a>
          </div>
          <div class="rrhh-person-file-meta">
            <span>Dependencia o AP: <strong>${f.departamento || "N/A"}</strong></span>
            <span>Estatus: <strong>${f.estatus || f.estado || "N/A"}</strong></span>
            <span>Ubicación: <strong>${f.ubicacion || "N/A"}</strong></span>
          </div>
        </div>
      `).join("")}
    </div>
  `).join("");
}

function openDocMetadataModal(idxReal) {
  if (!state.activePersonProfile?.rows) return;
  const doc = state.activePersonProfile.rows.find(d => d.__idx == idxReal);
  if (!doc) return;

  document.getElementById("modal-doc-title").innerText       = `${doc.doc_type} - ${doc.empleado}`;
  document.getElementById("modal-doc-thumb-icon").className  = "fas fa-id-card";
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type;

  document.getElementById("modal-doc-meta-container").innerHTML = `
    <div class="ds-doc-meta-row"><span class="k">Titular</span><span class="v">${doc.empleado}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Cédula</span><span class="v">${doc.cedula}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Personas vinculadas</span><span class="v">${doc.personas_relacionadas || "N/A"}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Tipo de Archivo</span><span class="v">${doc.doc_type}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Ubicación Física</span><span class="v">${doc.ubicacion}</span></div>
    <div class="ds-doc-meta-row"><span class="k">Fecha de Ingreso</span><span class="v">${formatISOToSpanish(doc.fecha_ingreso)}</span></div>
  `;
  document.getElementById("modal-doc-abstract").innerText =
    `Expediente Laboral Digitalizado del empleado ${doc.empleado}. Clasificado en el departamento de ${doc.departamento} con el estado de personal ${doc.estado || doc.estatus || "N/A"}.`;

  closeDocViewer();
  const fileUrl = doc.file_url || "";
  const viewBtn = document.getElementById("btn-modal-view");
  if (fileUrl) {
    viewBtn.innerHTML = '<i class="fas fa-file-pdf mr-1"></i>Ver PDF';
    viewBtn.onclick = () => toggleDocViewer(fileUrl);
  } else if ((doc.ubicacion || "").toLowerCase().includes("digitalizado")) {
    viewBtn.innerHTML = '<i class="fas fa-search mr-1"></i>Digitalizado';
    viewBtn.onclick = () => alert("Documento registrado como digitalizado pero sin URL de archivo asignada.\nContacte al administrador para vincular el archivo digital.");
  } else {
    viewBtn.innerHTML = '<i class="fas fa-map-marker-alt mr-1"></i>Ubicación';
    viewBtn.onclick = () => alert(`Archivo físico disponible en:\n${doc.ubicacion}`);
  }

  $("#doc-modal").modal("show");
}
