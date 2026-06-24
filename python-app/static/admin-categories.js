// =============================================================================
// ADMIN — Tipologías (Categorías) y Palabras Clave
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, isArchivoModule,
//             showToast, loadDynamicChoices, loadAdminTab)
// =============================================================================

function loadCategoriesTab() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`admin_tax_list-${suf}`);
  if (!state.choices) {
    if (container) container.innerHTML = `<div class="text-muted p-2">Sin tipologías activas.</div>`;
    return;
  }

  if (isArchivoModule()) {
    const types = state.choices.archivo.doc_types;
    if (container) container.innerHTML = types.length
      ? types.map(t => `
          <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm" style="border-left:4px solid #ffc107!important;">
            <h6 class="font-weight-bold text-dark mb-0">${t}</h6>
            <span class="badge badge-warning badge-pill">Archivo</span>
          </div>`).join("")
      : `<div class="text-muted p-2">Sin tipologías registradas.</div>`;
    loadKeywordsSection();
  } else {
    const tiposPorParte = state.choices.rrhh?.tipos_por_parte || {};
    const parteColors   = { "Parte I": "#0056b3", "Parte II": "#28a745", "Parte III": "#e67e22", "Parte IV": "#dc3545" };
    const parteEntries  = Object.entries(tiposPorParte);

    if (container) {
      if (parteEntries.length === 0) {
        const types = state.choices.rrhh.doc_types;
        container.innerHTML = types.map(t => `
          <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm" style="border-left:4px solid #6c757d!important;">
            <h6 class="font-weight-bold text-dark mb-0">${t}</h6>
            <span class="badge badge-secondary badge-pill">RRHH</span>
          </div>`).join("");
      } else {
        container.innerHTML = parteEntries.map(([parte, tipos]) => {
          const color = parteColors[parte] || "#6c757d";
          return `
            <div class="mb-3">
              <h6 class="px-2 py-1 rounded text-white font-weight-bold" style="background:${color};font-size:0.82rem;">${parte}</h6>
              ${tipos.map(t => `
                <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm py-1" style="border-left:4px solid ${color}!important;">
                  <span style="font-size:0.82rem;font-weight:600;">${t}</span>
                  <span class="badge badge-pill text-white" style="background:${color};font-size:0.7rem;">Activa</span>
                </div>`).join("")}
            </div>`;
        }).join("");
      }
    }
  }
}

async function loadKeywordsSection() {
  const suf     = adminSuffixFromTab();
  const catPane = document.getElementById(`pane-admin-${suf}-categories`);
  if (!catPane) return;

  let kwSection = document.getElementById("admin-keywords-section");
  if (!kwSection) {
    kwSection = document.createElement("div");
    kwSection.id = "admin-keywords-section";
    kwSection.className = "mt-4";
    catPane.appendChild(kwSection);
  }

  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords`);
    if (!res.ok) throw new Error();
    const keywords = await res.json();

    kwSection.innerHTML = `
      <div class="card card-info">
        <div class="card-header"><h3 class="card-title"><i class="fas fa-key mr-2"></i>Palabras Clave (Descriptores Libres)</h3></div>
        <div class="card-body p-3">
          <div class="input-group mb-3" style="max-width:420px;">
            <input type="text" id="new_keyword_input" class="form-control form-control-sm" placeholder="Ej: Gestión académica">
            <div class="input-group-append">
              <button class="btn btn-info btn-sm" onclick="handleAddKeyword()"><i class="fas fa-plus"></i> Agregar</button>
            </div>
          </div>
          <div class="row">
            ${keywords.length === 0
              ? `<div class="col-12 text-muted small">Sin palabras clave registradas aún.</div>`
              : keywords.map(kw => `
                  <div class="col-md-4 col-sm-6 mb-2" id="kw-item-${kw.id}">
                    <div class="d-flex align-items-center border rounded px-2 py-1 bg-white shadow-sm">
                      <i class="fas fa-tag text-info mr-2" style="font-size:0.78rem;"></i>
                      <span class="flex-grow-1 font-weight-600" style="font-size:0.82rem;" id="kw-label-${kw.id}">${kw.nombre}</span>
                      <small class="text-muted mr-1">(${kw.uso_archivo})</small>
                      <button class="btn btn-link btn-sm p-0 mr-1" onclick="handleEditKeyword(${kw.id})" title="Renombrar">
                        <i class="fas fa-pen text-warning" style="font-size:0.72rem;"></i>
                      </button>
                      <button class="btn btn-link btn-sm p-0" onclick="handleDeleteKeyword(${kw.id},'${kw.nombre.replace(/'/g,"\\'")}')" title="Eliminar">
                        <i class="fas fa-trash text-danger" style="font-size:0.72rem;"></i>
                      </button>
                    </div>
                  </div>`).join("")}
          </div>
        </div>
      </div>`;
  } catch (e) {
    console.error("Error cargando palabras clave:", e);
    if (kwSection) kwSection.innerHTML = `<div class="alert alert-warning">No se pudo cargar las palabras clave.</div>`;
  }
}

async function handleAddKeyword() {
  const input  = document.getElementById("new_keyword_input");
  const nombre = (input?.value || "").trim();
  if (!nombre) { showToast("Ingrese una palabra clave.", "warning"); return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre }),
    });
    if (!res.ok) {
      const err = await res.json().catch(() => ({}));
      throw new Error(err.detail || "Error");
    }
    if (input) input.value = "";
    loadKeywordsSection();
  } catch (err) {
    showToast(err.message || "Error al agregar palabra clave.", "error");
  }
}

async function handleEditKeyword(id) {
  const labelEl     = document.getElementById(`kw-label-${id}`);
  const currentName = labelEl?.innerText || "";
  const newName     = typeof promptModal === "function"
    ? await promptModal("Renombrar palabra clave", "Nuevo nombre", currentName, "Nombre de la palabra clave...")
    : prompt("Nuevo nombre para la palabra clave:", currentName);
  if (!newName || newName.trim() === currentName) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords/${id}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre: newName.trim() }),
    });
    if (!res.ok) throw new Error();
    loadKeywordsSection();
  } catch {
    showToast("Error al renombrar la palabra clave.", "error");
  }
}

async function handleDeleteKeyword(id, nombre) {
  if (!confirm(`¿Eliminar la palabra clave "${nombre}"?\nSe desvinculará de todos los documentos que la usen.`)) return;
  try {
    const res = await fetch(`${API_BASE}/api/admin/keywords/${id}`, { method: "DELETE" });
    if (!res.ok) throw new Error();
    loadKeywordsSection();
  } catch {
    showToast("Error al eliminar la palabra clave.", "error");
  }
}

async function handleAddCategory() {
  const suf   = adminSuffixFromTab();
  const name  = document.getElementById(`new_tax_name-${suf}`)?.value.trim() || "";
  const desc  = document.getElementById(`new_tax_desc-${suf}`)?.value.trim() || "";
  const scope = isArchivoModule() ? "Archivo" : "RRHH";
  const parte = document.getElementById(`new_tax_parte-${suf}`)?.value || "";
  if (!name) { showToast("Por favor, ingrese el nombre de la tipología.", "warning"); return; }
  try {
    const res = await fetch(`${API_BASE}/api/admin/add_category`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, desc, scope, parte, usuario: state.user.username }),
    });
    if (!res.ok) throw new Error();
    showToast("¡Nueva tipología guardada con éxito!", "success");
    if (document.getElementById(`new_tax_name-${suf}`)) document.getElementById(`new_tax_name-${suf}`).value = "";
    if (document.getElementById(`new_tax_desc-${suf}`)) document.getElementById(`new_tax_desc-${suf}`).value = "";
    await loadDynamicChoices();
    loadAdminTab("categories");
  } catch {
    showToast("Error al guardar tipología.", "error");
  }
}
