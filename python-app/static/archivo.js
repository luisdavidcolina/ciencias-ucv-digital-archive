// ==========================================================================
// BÚSQUEDA Y RENDER — ARCHIVO INSTITUCIONAL
// ==========================================================================
async function triggerArchivoSearch() {
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
        sort_mode:     state.archivo.sortMode
      })
    });
    if (!res.ok) throw new Error();
    state.archivo.results = await res.json();
    renderArchivoList();
  } catch (e) {
    console.error("Error buscando archivo:", e);
  }
}

function renderArchivoList() {
  const container = document.getElementById("list_archivo");
  const results   = state.archivo.results;
  document.getElementById("count-archivo-results").innerText = `${results.length} Registros`;

  if (results.length === 0) {
    container.innerHTML = `<div class="alert alert-secondary text-center p-4">
      <i class="fas fa-search fa-2x mb-2 text-muted"></i>
      <p class="mb-0">No se encontraron folios correspondientes a los criterios ingresados.</p>
    </div>`;
    document.getElementById("info-archivo-pagination").innerText = "Pág 1 de 1";
    return;
  }

  const totalPages = Math.ceil(results.length / state.archivo.perPage);
  if (state.archivo.page > totalPages) state.archivo.page = totalPages;
  const start     = (state.archivo.page - 1) * state.archivo.perPage;
  const pageItems = results.slice(start, start + state.archivo.perPage);
  document.getElementById("info-archivo-pagination").innerText = `Pág ${state.archivo.page} de ${totalPages}`;

  container.innerHTML = pageItems.map(doc => `
    <div class="ds-item-card" onclick="openArchivoModal('${doc.__idx}')" style="cursor:pointer;">
      <div class="ds-item-thumbnail">
        <i class="fas fa-file-alt" style="font-size:40px;color:#2b4e72;"></i>
      </div>
      <div class="ds-item-metadata" style="flex-grow:1;padding-left:15px;">
        <div class="d-flex justify-content-between align-items-center mb-1">
          <span class="badge badge-light" style="font-size:0.75rem;color:#2b4e72;font-weight:bold;border:1px solid #d9e6f4;border-radius:12px;padding:3px 10px;">
            <i class="fas fa-bookmark mr-1"></i> ${doc.doc_type}
          </span>
          <span class="text-muted" style="font-size:0.8rem;"><i class="far fa-calendar-alt mr-1"></i> ${formatISOToSpanish(doc.fecha)}</span>
        </div>
        <h4 class="ds-item-title" style="font-size:1.1rem;font-weight:700;color:#2b4e72;margin:4px 0;">${doc.titulo}</h4>
        <div class="ds-item-authors" style="font-size:0.82rem;color:#495057;margin-bottom:2px;">
          <i class="fas fa-user-edit mr-1"></i> Autor: <strong>${doc.autor}</strong>
        </div>
        <div class="ds-item-publisher" style="font-size:0.82rem;color:#6c757d;margin-bottom:4px;">
          <i class="fas fa-map-marker-alt mr-1"></i> Ubicación: <strong>${doc.ubicacion}</strong>
        </div>
        ${doc.resumen ? `<p class="ds-item-abstract text-muted m-0 mt-1" style="font-size:0.82rem;line-height:1.4;display:-webkit-box;-webkit-line-clamp:2;-webkit-box-orient:vertical;overflow:hidden;">${doc.resumen}</p>` : ""}
        <div class="ds-item-badges d-flex flex-wrap gap-1 mt-2">
          ${doc.tesauro_badges.slice(0, 4).map(b => `<span class="badge" style="background-color:#2b4e72;color:white;font-size:0.7rem;padding:2px 7px;border-radius:4px;margin-right:4px;">${b}</span>`).join("")}
          ${doc.tesauro_badges.length > 4 ? `<span class="badge" style="background-color:#6c757d;color:white;font-size:0.7rem;padding:2px 7px;border-radius:4px;">+${doc.tesauro_badges.length - 4}</span>` : ""}
        </div>
      </div>
      <div class="ds-item-actions" style="margin-left:15px;display:flex;flex-direction:column;justify-content:center;">
        <button class="btn btn-primary ds-action-btn" title="Ver"
          onclick="event.stopPropagation();openArchivoModal('${doc.__idx}')"
          style="width:36px;height:36px;border-radius:50%!important;display:inline-flex;align-items:center;justify-content:center;">
          <i class="fas fa-eye"></i>
        </button>
      </div>
    </div>
  `).join("");
}

function openArchivoModal(idxReal) {
  const doc = state.archivo.results.find(d => d.__idx == idxReal);
  if (!doc) return;

  document.getElementById("modal-doc-title").innerText       = doc.titulo;
  document.getElementById("modal-doc-thumb-icon").className  = "fas fa-file-alt";
  document.getElementById("modal-doc-thumb-badge").innerText = doc.doc_type;

  document.getElementById("modal-doc-meta-container").innerHTML = `
    <div class="ds-doc-meta-row"><span class="k">dc.title</span><span class="v">${doc.titulo}</span></div>
    <div class="ds-doc-meta-row"><span class="k">dc.contributor.author</span><span class="v">${doc.autor}</span></div>
    <div class="ds-doc-meta-row"><span class="k">dc.date.issued</span><span class="v">${formatISOToSpanish(doc.fecha)}</span></div>
    <div class="ds-doc-meta-row"><span class="k">dc.type</span><span class="v">${doc.doc_type}</span></div>
    <div class="ds-doc-meta-row"><span class="k">dc.identifier.location</span><span class="v">${doc.ubicacion}</span></div>
    <div class="ds-doc-meta-row"><span class="k">dc.subject.classification</span><span class="v">${doc.tesauro_badges.join("; ")}</span></div>
  `;
  document.getElementById("modal-doc-abstract").innerText =
    doc.resumen || "No se ha registrado un resumen descriptivo abstracto (dc.description.abstract) para este folio.";

  document.getElementById("btn-modal-view").onclick = () =>
    alert(`Visualizando documento: ${doc.titulo}\nUbicado físicamente en: ${doc.ubicacion}`);
  document.getElementById("btn-modal-download").classList.add("d-none");
  document.getElementById("btn-modal-edit").classList.add("d-none");

  $("#doc-modal").modal("show");
}
