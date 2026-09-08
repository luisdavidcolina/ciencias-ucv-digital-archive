// =============================================================================
// ADMIN — Tipos de Documento y Palabras Clave
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, isArchivoModule,
//             showToast, loadDynamicChoices, loadAdminTab, formatAnios —
//             VI-028), app.js (escHtml)
//
// Nomenclatura (CLAUDE.md): nunca "Tesauro". Aquí, además, un solo nombre para
// el mismo concepto en toda la interfaz que este archivo controla: "tipo
// documental" para tipo_documento y "Parte" reservado para la agrupación de
// RRHH (OA-054, OR-171).
// =============================================================================

const PARTES_CANONICAS = ["Parte I", "Parte II", "Parte III", "Parte IV"];
const PARTE_COLORS = {
  "Parte I": "#0056b3",
  "Parte II": "#28a745",
  "Parte III": "#e67e22",
  "Parte IV": "#dc3545",
  "Sin clasificar": "#6c757d",
};
const CAT_NAME_MAX_LENGTH = 150;

// Caché en memoria de los tipos ya cargados por módulo (archivo/rrhh), usada
// para buscar, ordenar y validar duplicados en el cliente sin volver a pedir
// el catálogo completo (OA-127, OR-169, OR-170).
const _catCache = {};
const _catFilter = {};

function loadCategoriesTab() {
  const suf    = adminSuffixFromTab();
  const scope  = isArchivoModule() ? "archivo" : "rrhh";
  const container = document.getElementById(`admin_tax_list-${suf}`);
  if (!container) return;

  _ensureCatToolbar(container, suf);
  container.innerHTML = `<div class="text-muted p-2"><i class="fas fa-spinner fa-spin mr-2"></i>Cargando tipos documentales...</div>`;

  apiFetchJSON(`${API_BASE}/api/admin/retencion/tipos?scope=${scope}`)
    .then(data => {
      _catCache[suf] = data.tipos || [];
      _renderCatList(suf);
    })
    .catch(e => {
      console.error("Error cargando tipos documentales:", e);
      _catCache[suf] = null;
      container.innerHTML = `
        <div class="alert alert-warning d-flex justify-content-between align-items-center mb-0">
          <span>No se pudo cargar la lista de tipos documentales.</span>
          <button type="button" class="btn btn-sm btn-outline-secondary" onclick="loadCategoriesTab()">
            <i class="fas fa-redo mr-1"></i>Reintentar
          </button>
        </div>`;
    });

  loadKeywordsSection();
}

// Caja de búsqueda (y, en Archivo, orden por uso) por encima de la lista.
// Se inserta una sola vez como hermano del contenedor de la lista, para que
// sobreviva a los redibujados de `_renderCatList` (OA-127, OR-170).
function _ensureCatToolbar(container, suf) {
  const toolbarId = `admin_tax_toolbar-${suf}`;
  if (document.getElementById(toolbarId)) return;

  const scope = isArchivoModule() ? "archivo" : "rrhh";
  const sortControl = scope === "archivo"
    ? `<select id="admin_tax_sort-${suf}" class="form-control form-control-sm ml-2" style="max-width:170px;" onchange="_renderCatList('${suf}')">
         <option value="nombre">Ordenar: Nombre</option>
         <option value="uso">Ordenar: Uso</option>
       </select>`
    : "";

  const toolbar = document.createElement("div");
  toolbar.id = toolbarId;
  toolbar.className = "d-flex mb-2";
  toolbar.innerHTML = `
    <input type="text" id="admin_tax_search-${suf}" class="form-control form-control-sm"
           placeholder="Buscar tipo documental..." oninput="_renderCatList('${suf}')">
    ${sortControl}`;
  container.parentElement.insertBefore(toolbar, container);
}

function _renderCatList(suf) {
  const container = document.getElementById(`admin_tax_list-${suf}`);
  if (!container) return;
  const tipos = _catCache[suf];
  if (!tipos) return; // el error ya se pintó en loadCategoriesTab

  const searchEl = document.getElementById(`admin_tax_search-${suf}`);
  const query = (searchEl?.value || "").trim().toLowerCase();
  _catFilter[suf] = query;

  const filtered = query
    ? tipos.filter(t => (t.nombre_corto || t.nombre || "").toLowerCase().includes(query))
    : tipos.slice();

  if (tipos.length === 0) {
    container.innerHTML = `<div class="text-muted p-2">Sin tipos documentales registrados.</div>`;
    return;
  }
  if (filtered.length === 0) {
    container.innerHTML = `<div class="text-muted p-2">Ningún tipo documental coincide con «${escHtml(query)}».</div>`;
    return;
  }

  container.innerHTML = isArchivoModule()
    ? _renderCatListArchivo(suf, filtered)
    : _renderCatListRRHH(filtered);
}

function _catUsoBadge(uso) {
  return uso > 0
    ? `<span class="badge badge-info badge-pill" title="Documentos que usan este tipo">${uso} en uso</span>`
    : `<span class="badge badge-light border text-muted" title="Ningún documento usa este tipo">sin uso</span>`;
}

function _renderCatListArchivo(suf, tipos) {
  const sortEl = document.getElementById(`admin_tax_sort-${suf}`);
  const sortBy = sortEl?.value || "nombre";
  const sorted = tipos.slice().sort((a, b) => sortBy === "uso"
    ? Number(b.uso_archivo || 0) - Number(a.uso_archivo || 0)
    : (a.nombre_corto || a.nombre || "").localeCompare(b.nombre_corto || b.nombre || "", "es"));

  return sorted.map(t => {
    // VI-033: un tipo documental sin nombre se pintaba como una fila alta y
    // vacía, sin nada que diga qué se está editando. Respaldo explícito.
    const nombreLegible = t.nombre_corto || t.nombre;
    const nombreDisplay = nombreLegible
      ? escHtml(nombreLegible)
      : `<span class="text-muted font-italic" title="Tipo documental sin nombre registrado">(sin nombre — #${t.id})</span>`;
    return `
    <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm" style="border-left:4px solid #ffc107!important;">
      <div>
        <h6 class="font-weight-bold text-dark mb-0">${nombreDisplay}</h6>
        <small class="text-muted">Retención: ${formatAnios(t.plazo_retencion_anios)}</small>
      </div>
      ${_catUsoBadge(Number(t.uso_archivo || 0))}
    </div>`;
  }).join("");
}

function _renderCatListRRHH(tipos) {
  const byParte = {};
  for (const parte of PARTES_CANONICAS) byParte[parte] = [];
  byParte["Sin clasificar"] = [];

  for (const t of tipos) {
    const key = PARTES_CANONICAS.includes(t.categoria) ? t.categoria : "Sin clasificar";
    byParte[key].push(t);
  }

  const groups = [...PARTES_CANONICAS, "Sin clasificar"];
  return groups.map(parte => {
    const items = byParte[parte].slice().sort((a, b) =>
      (a.nombre_corto || a.nombre || "").localeCompare(b.nombre_corto || b.nombre || "", "es"));
    const color = PARTE_COLORS[parte];
    const body = items.length
      ? items.map(t => {
          // VI-033: mismo respaldo que en Archivo — sin nombre no puede
          // quedar una fila vacía sin decir qué se está editando.
          const nombreLegible = t.nombre_corto || t.nombre;
          const nombreDisplay = nombreLegible
            ? escHtml(nombreLegible)
            : `<span class="text-muted font-italic" title="Tipo documental sin nombre registrado">(sin nombre — #${t.id})</span>`;
          return `
          <div class="list-group-item d-flex justify-content-between align-items-center mb-1 rounded bg-white shadow-sm py-1" style="border-left:4px solid ${color}!important;">
            <div>
              <span style="font-size:0.82rem;font-weight:600;">${nombreDisplay}</span>
              <br><small class="text-muted" style="font-size:0.72rem;">Retención: ${formatAnios(t.plazo_retencion_anios)}</small>
            </div>
            ${_catUsoBadge(Number(t.uso_rrhh || 0))}
          </div>`;
        }).join("")
      : `<div class="text-muted small px-2 pb-2">Sin tipos documentales en esta Parte.</div>`;
    return `
      <div class="mb-3">
        <h6 class="px-2 py-1 rounded text-white font-weight-bold" style="background:${color};font-size:0.82rem;">${escHtml(parte)}</h6>
        ${body}
      </div>`;
  }).join("");
}

async function loadKeywordsSection() {
  const suf     = adminSuffixFromTab();
  const catPane = document.getElementById(`pane-admin-${suf}-categories`);
  if (!catPane) return;

  // El id lleva sufijo de módulo: en un admin Global que navega entre paneles,
  // dos secciones sin sufijo se pisarían entre sí (OA-030).
  let kwSection = document.getElementById(`admin-keywords-section-${suf}`);
  if (!kwSection) {
    kwSection = document.createElement("div");
    kwSection.id = `admin-keywords-section-${suf}`;
    kwSection.className = "mt-4";
    catPane.appendChild(kwSection);
  }

  try {
    const keywords = await apiFetchJSON(`${API_BASE}/api/admin/keywords`);
    kwSection.dataset.loaded = "1";
    kwSection.innerHTML = `
      <div class="card card-info">
        <div class="card-header"><h3 class="card-title"><i class="fas fa-key mr-2"></i>Palabras Clave (Descriptores Libres)</h3></div>
        <div class="card-body p-3">
          <div class="input-group mb-3" style="max-width:420px;">
            <input type="text" id="new_keyword_input-${suf}" class="form-control form-control-sm" placeholder="Ej: Gestión académica">
            <div class="input-group-append">
              <button class="btn btn-info btn-sm" onclick="handleAddKeyword('${suf}')"><i class="fas fa-plus"></i> Agregar</button>
            </div>
          </div>
          <div class="row" id="kw-list-${suf}">
            ${keywords.length === 0
              ? `<div class="col-12 text-muted small" id="kw-empty-${suf}">Sin palabras clave registradas aún.</div>`
              : keywords.map(kw => _renderKeywordItem(kw)).join("")}
          </div>
        </div>
      </div>`;
  } catch (e) {
    console.error("Error cargando palabras clave:", e);
    if (kwSection) {
      kwSection.innerHTML = `
        <div class="alert alert-warning d-flex justify-content-between align-items-center mb-0">
          <span>No se pudo cargar las palabras clave.</span>
          <button type="button" class="btn btn-sm btn-outline-secondary" onclick="loadKeywordsSection()">
            <i class="fas fa-redo mr-1"></i>Reintentar
          </button>
        </div>`;
    }
  }
}

function _renderKeywordItem(kw) {
  return `
    <div class="col-md-4 col-sm-6 mb-2" id="kw-item-${kw.id}">
      <div class="d-flex align-items-center border rounded px-2 py-1 bg-white shadow-sm">
        <i class="fas fa-tag text-info mr-2" style="font-size:0.78rem;"></i>
        <span class="flex-grow-1 font-weight-600" style="font-size:0.82rem;" id="kw-label-${kw.id}">${escHtml(kw.nombre)}</span>
        <small class="text-muted mr-1">(${Number(kw.uso_archivo || 0)})</small>
        <button class="btn btn-link btn-sm p-0 mr-1" onclick="handleEditKeyword(${kw.id})" title="Renombrar">
          <i class="fas fa-pen text-warning" style="font-size:0.72rem;"></i>
        </button>
        <button class="btn btn-link btn-sm p-0" onclick="handleDeleteKeyword(${kw.id},${escHtml(JSON.stringify(kw.nombre))},${Number(kw.uso_archivo || 0)})" title="Eliminar">
          <i class="fas fa-trash text-danger" style="font-size:0.72rem;"></i>
        </button>
      </div>
    </div>`;
}

// Añadir, renombrar o borrar una palabra clave actualiza sólo el nodo
// afectado en vez de recargar la lista entera (OA-129): con cientos de
// descriptores, `loadKeywordsSection()` completo perdía el desplazamiento y
// el foco en cada operación.
async function handleAddKeyword(suf) {
  suf = suf || adminSuffixFromTab();
  const input  = document.getElementById(`new_keyword_input-${suf}`);
  const nombre = (input?.value || "").trim();
  if (!nombre) { showToast("Ingrese una palabra clave.", "warning"); return; }
  try {
    const res = await apiFetchJSON(`${API_BASE}/api/admin/keywords`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre }),
    });
    if (input) input.value = "";
    const list = document.getElementById(`kw-list-${suf}`);
    const empty = document.getElementById(`kw-empty-${suf}`);
    if (empty) empty.remove();
    if (list && res?.id) {
      list.insertAdjacentHTML("beforeend", _renderKeywordItem({ id: res.id, nombre, uso_archivo: 0 }));
    } else {
      loadKeywordsSection();
    }
    showToast(`Palabra clave "${nombre}" agregada.`, "success");
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
  const trimmed = newName.trim();
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/keywords/${id}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ nombre: trimmed }),
    });
    if (labelEl) labelEl.textContent = trimmed;
    showToast("Palabra clave renombrada.", "success");
  } catch (err) {
    showToast(err.message || "Error al renombrar la palabra clave.", "error");
  }
}

async function handleDeleteKeyword(id, nombre, usoCount) {
  const msg = usoCount > 0
    ? `La palabra clave "${nombre}" está en uso en ${usoCount} documento(s). ¿Eliminar y desvincular de todos?`
    : `¿Eliminar la palabra clave "${nombre}"?`;
  const btnClass = usoCount > 0 ? "btn-danger" : "btn-warning";
  const confirmed = typeof confirmModal === "function"
    ? await confirmModal("Eliminar Palabra Clave", msg, "Eliminar", btnClass)
    : confirm(msg);
  if (!confirmed) return;
  try {
    const force = usoCount > 0 ? "?force=true" : "";
    await apiFetchJSON(`${API_BASE}/api/admin/keywords/${id}${force}`, { method: "DELETE" });
    showToast(`Palabra clave "${nombre}" eliminada.`, "success");
    const item = document.getElementById(`kw-item-${id}`);
    if (item) {
      const list = item.parentElement;
      item.remove();
      if (list && !list.children.length) {
        const suf = adminSuffixFromTab();
        list.insertAdjacentHTML("beforeend", `<div class="col-12 text-muted small" id="kw-empty-${suf}">Sin palabras clave registradas aún.</div>`);
      }
    }
  } catch (err) {
    showToast(err.message || "Error al eliminar la palabra clave.", "error");
  }
}

async function handleAddCategory() {
  const suf   = adminSuffixFromTab();
  const nameInput = document.getElementById(`new_tax_name-${suf}`);
  const descInput = document.getElementById(`new_tax_desc-${suf}`);
  const name  = (nameInput?.value || "").trim();
  const desc  = (descInput?.value || "").trim();
  const scope = isArchivoModule() ? "Archivo" : "RRHH";
  const parte = document.getElementById(`new_tax_parte-${suf}`)?.value || "";

  if (!name) { showToast("Por favor, ingrese el nombre del tipo documental.", "warning"); return; }
  if (name.length > CAT_NAME_MAX_LENGTH) {
    showToast(`El nombre no puede superar los ${CAT_NAME_MAX_LENGTH} caracteres.`, "warning");
    return;
  }
  // Validación en vivo contra lo ya cargado (OR-169): el servidor vuelve a
  // comprobarlo, esto sólo evita el viaje y el mensaje ambiguo del backend.
  const existing = _catCache[suf] || [];
  const dupe = existing.some(t => (t.nombre_corto || t.nombre || "").trim().toLowerCase() === name.toLowerCase());
  if (dupe) {
    showToast(`Ya existe un tipo documental llamado "${name}".`, "warning");
    return;
  }

  try {
    await apiFetchJSON(`${API_BASE}/api/admin/add_category`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, desc, scope, parte, usuario: state.user.username }),
    });
    showToast("Tipo documental guardado con éxito.", "success");
    if (nameInput) nameInput.value = "";
    if (descInput) descInput.value = "";
    await loadDynamicChoices();
    loadAdminTab("categories");
  } catch (err) {
    showToast(err.message || "Error al guardar el tipo documental.", "error");
  }
}
