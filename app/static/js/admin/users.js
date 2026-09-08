// =============================================================================
// ADMIN — Gestión de Usuarios y Auditoría
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, showToast, loadAdminTab)
// =============================================================================

let auditState = { page: 1, perPage: 50, total: 0 };

// Copia local de los usuarios ya cargados por pestaña ("archivo" / "rrhh"),
// para poder filtrar/ordenar sin volver a pedirle la lista al backend.
const usersState = {
  archivo: { raw: [], search: "", rol: "", estado: "", sortBy: "usuario", sortDir: "asc" },
  rrhh:    { raw: [], search: "", rol: "", estado: "", sortBy: "usuario", sortDir: "asc" },
};

const STALE_DAYS = 90; // OA-165: umbral para marcar cuentas sin acceso reciente

function _daysSince(iso) {
  if (!iso) return null;
  const d = new Date(iso);
  if (isNaN(d)) return null;
  return (Date.now() - d.getTime()) / 86400000;
}

async function loadUsersTab() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`admin_users_table-${suf}`);
  if (!container) return; // OA-162: el pane puede no estar montado todavía
  const moduloFiltro = suf === "rrhh" ? "RRHH" : "Archivo";

  container.innerHTML = `<div class="text-center text-muted p-3"><i class="fas fa-spinner fa-spin mr-1"></i>Cargando usuarios...</div>`;

  try {
    const users = await apiFetchJSON(`${API_BASE}/api/admin/users?modulo=${encodeURIComponent(moduloFiltro)}`);
    usersState[suf].raw = Array.isArray(users) ? users : [];
    renderUsersTable(suf);
  } catch (e) {
    container.innerHTML = `
      <div class="alert alert-danger d-flex align-items-center justify-content-between">
        <span><i class="fas fa-exclamation-triangle mr-1"></i>Error al cargar listado de seguridad.</span>
        <button class="btn btn-sm btn-outline-danger" onclick="loadUsersTab()"><i class="fas fa-redo mr-1"></i>Reintentar</button>
      </div>`;
  }
}

// OA-165: buscador, filtro por rol/estado y orden — todo en cliente, sobre
// la lista ya traída, así los botones de acción no vuelven a pedir nada.
function renderUsersTable(suf) {
  const container = document.getElementById(`admin_users_table-${suf}`);
  if (!container) return;
  const st = usersState[suf];

  const term = st.search.trim().toLowerCase();
  let rows = st.raw.filter(u => {
    if (term && !String(u.usuario || "").toLowerCase().includes(term)) return false;
    if (st.rol && u.rol !== st.rol) return false;
    if (st.estado === "activo" && u.is_active === false) return false;
    if (st.estado === "inactivo" && u.is_active !== false) return false;
    return true;
  });

  rows = rows.slice().sort((a, b) => {
    let av, bv;
    if (st.sortBy === "last_login") {
      av = a.last_login ? new Date(a.last_login).getTime() : -Infinity;
      bv = b.last_login ? new Date(b.last_login).getTime() : -Infinity;
    } else {
      av = String(a.usuario || "").toLowerCase();
      bv = String(b.usuario || "").toLowerCase();
    }
    const cmp = av < bv ? -1 : av > bv ? 1 : 0;
    return st.sortDir === "desc" ? -cmp : cmp;
  });

  const roles = [...new Set(st.raw.map(u => u.rol).filter(Boolean))];

  container.innerHTML = `
    <div class="form-row align-items-center mb-2">
      <div class="col-sm-4 mb-1">
        <input type="text" class="form-control form-control-sm" placeholder="Buscar usuario..."
               value="${escHtml(st.search)}"
               oninput="usersState['${suf}'].search=this.value; renderUsersTable('${suf}')">
      </div>
      <div class="col-sm-3 mb-1">
        <select class="form-control form-control-sm" onchange="usersState['${suf}'].rol=this.value; renderUsersTable('${suf}')">
          <option value="">Todos los roles</option>
          ${roles.map(r => `<option value="${escHtml(r)}" ${st.rol === r ? "selected" : ""}>${escHtml(r)}</option>`).join("")}
        </select>
      </div>
      <div class="col-sm-3 mb-1">
        <select class="form-control form-control-sm" onchange="usersState['${suf}'].estado=this.value; renderUsersTable('${suf}')">
          <option value="">Todos los estados</option>
          <option value="activo" ${st.estado === "activo" ? "selected" : ""}>Activos</option>
          <option value="inactivo" ${st.estado === "inactivo" ? "selected" : ""}>Inactivos</option>
        </select>
      </div>
      <div class="col-sm-2 mb-1 text-sm-right">
        <span class="text-muted small">${rows.length} de ${st.raw.length}</span>
      </div>
    </div>
    <table class="table table-striped table-bordered" style="font-size:0.85rem;">
      <thead>
        <tr class="bg-light">
          <th role="button" onclick="_toggleUsersSort('${suf}','usuario')">Usuario ${_sortArrow(st, 'usuario')}</th>
          <th>Módulo</th>
          <th>Rol</th>
          <th>Estado</th>
          <th role="button" onclick="_toggleUsersSort('${suf}','last_login')">Último Acceso ${_sortArrow(st, 'last_login')}</th>
          <th>Acciones</th>
        </tr>
      </thead>
      <tbody>
        ${rows.length === 0
          ? `<tr><td colspan="6" class="text-center text-muted p-3">Sin usuarios que coincidan con el filtro.</td></tr>`
          : rows.map(u => _renderUserRow(u)).join("")}
      </tbody>
    </table>
  `;
}

function _sortArrow(st, field) {
  if (st.sortBy !== field) return "";
  return st.sortDir === "asc" ? "▲" : "▼";
}

function _toggleUsersSort(suf, field) {
  const st = usersState[suf];
  if (st.sortBy === field) st.sortDir = st.sortDir === "asc" ? "desc" : "asc";
  else { st.sortBy = field; st.sortDir = "asc"; }
  renderUsersTable(suf);
}

function _renderUserRow(u) {
  const isActive  = u.is_active !== false;
  const daysStale = _daysSince(u.last_login);
  const isStale   = daysStale !== null && daysStale > STALE_DAYS;
  const lastLoginText  = u.last_login ? formatRelativeTime(u.last_login) : "Nunca";
  const lastLoginTitle = u.last_login ? formatISOToSpanish(u.last_login) : "";
  const uEsc = escHtml(u.usuario);

  // OA-163: el botón describe la acción, no el estado; el estado va en su
  // propia columna con role="switch" para que un lector de pantalla lo anuncie.
  return `
    <tr>
      <td class="font-weight-bold text-dark"><i class="fas fa-user-circle mr-1 text-secondary"></i> ${uEsc}</td>
      <td>${escHtml(u.modulo)}</td>
      <td><span class="badge ${u.rol === "Admin" ? "badge-danger" : "badge-primary"}">${escHtml(u.rol)}</span></td>
      <td>
        <button class="btn btn-xs ${isActive ? 'btn-success' : 'btn-secondary'}"
                role="switch" aria-checked="${isActive}"
                onclick="handleToggleUserActive(${u.id}, ${escHtml(JSON.stringify(u.usuario))}, ${isActive})"
                title="${isActive ? 'Cuenta activa — pulsar para desactivar' : 'Cuenta inactiva — pulsar para activar'}">
          <i class="fas fa-${isActive ? 'times-circle' : 'check-circle'}"></i>
          ${isActive ? 'Desactivar' : 'Activar'}
        </button>
      </td>
      <td class="text-muted small" title="${lastLoginTitle}">
        ${lastLoginText}
        ${isStale ? '<span class="badge badge-warning ml-1" title="Sin acceso en más de ' + STALE_DAYS + ' días">inactiva</span>' : ''}
      </td>
      <td style="white-space:nowrap;">
        <button class="btn btn-xs btn-outline-secondary mr-1" onclick="handleChangePassword(${u.id}, ${escHtml(JSON.stringify(u.usuario))})">
          <i class="fas fa-key"></i> Clave
        </button>
        <button class="btn btn-xs btn-outline-info mr-1" onclick="handleViewUserHistory(${escHtml(JSON.stringify(u.usuario))})" title="Ver historial de esta cuenta">
          <i class="fas fa-history"></i>
        </button>
        <button class="btn btn-xs btn-outline-danger" onclick="handleDeleteUser(${u.id}, ${escHtml(JSON.stringify(u.usuario))})">
          <i class="fas fa-trash-alt"></i>
        </button>
      </td>
    </tr>`;
}

async function handleToggleUserActive(uid, username, wasActive) {
  // Desactivar deja a alguien fuera del sistema al instante (OA-163):
  // pedir confirmación igual que para borrar, activar no hace falta.
  if (wasActive) {
    const ok = typeof confirmModal === "function"
      ? await confirmModal(`¿Desactivar a "${username}"?`, "Perderá acceso al sistema de inmediato.", "Desactivar", "warning")
      : confirm(`¿Desactivar a "${username}"? Perderá acceso al sistema de inmediato.`);
    if (!ok) return;
  }
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/users/${uid}/active?requester=${encodeURIComponent(state.user.username)}`, {
      method: "PATCH",
    });
    showToast(`Usuario "${username}" ${data.is_active ? 'activado' : 'desactivado'}.`, data.is_active ? "success" : "warning");
    loadUsersTab();
  } catch {
    showToast("Error al cambiar el estado del usuario.", "error");
  }
}

async function handleDeleteUser(uid, username) {
  const ok = typeof confirmModal === "function"
    ? await confirmModal(`¿Eliminar al usuario "${username}"?`, "Esta acción es irreversible.", "Eliminar", "danger")
    : confirm(`¿Eliminar al usuario "${username}"? Esta acción es irreversible.`);
  if (!ok) return;
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/users/${uid}?requester=${encodeURIComponent(state.user.username)}`, {
      method: "DELETE",
    });
    showToast(`Usuario "${username}" eliminado.`, "success");
    loadUsersTab();
  } catch {
    showToast("Error al eliminar el usuario.", "error");
  }
}

// OA-039: medidor de fortaleza. Puramente informativo en el cliente — el
// mínimo que de verdad se exige (hoy 6, `core/security.py`/`routes/admin/users.py`)
// no es parte de esta zona, así que no se toca el umbral de validación aquí.
function _pwdStrengthInfo(pw) {
  const val = pw || "";
  let variedad = 0;
  if (/[a-z]/.test(val)) variedad++;
  if (/[A-Z]/.test(val)) variedad++;
  if (/[0-9]/.test(val)) variedad++;
  if (/[^A-Za-z0-9]/.test(val)) variedad++;
  let score = 0;
  if (val.length >= 6) score++;
  if (val.length >= 10) score++;
  if (variedad >= 3) score++;
  if (val.length >= 14 && variedad >= 3) score++;
  score = Math.min(score, 4);
  const niveles = [
    { label: "Muy débil", clase: "bg-danger" },
    { label: "Débil", clase: "bg-danger" },
    { label: "Aceptable", clase: "bg-warning" },
    { label: "Fuerte", clase: "bg-info" },
    { label: "Muy fuerte", clase: "bg-success" },
  ];
  return { score, pct: val ? (score + 1) * 20 : 0, ...niveles[score] };
}

function _pwdStrengthMarkup() {
  return `
    <div class="ds-pwd-strength mt-1" aria-live="polite">
      <div class="progress" style="height:4px;">
        <div class="progress-bar" role="progressbar" style="width:0%" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100"></div>
      </div>
      <small class="text-muted ds-pwd-strength-label"></small>
    </div>`;
}

function _updatePwdStrengthMeter(meterEl, pw) {
  if (!meterEl) return;
  const info = _pwdStrengthInfo(pw);
  const bar   = meterEl.querySelector(".progress-bar");
  const label = meterEl.querySelector(".ds-pwd-strength-label");
  if (bar) {
    bar.style.width = `${info.pct}%`;
    bar.setAttribute("aria-valuenow", String(info.pct));
    bar.className = `progress-bar ${pw ? info.clase : ""}`;
  }
  if (label) label.textContent = pw ? `Fortaleza: ${info.label}` : "";
}

// El campo "Registrar Nuevo Usuario" lo inyecta admin-ui.js una sola vez
// (`_panelAcceso`, fuera de esta zona) y persiste en el DOM entre pestañas,
// así que se engancha por delegación en vez de tocar ese marcado.
document.addEventListener("input", e => {
  const input = e.target;
  if (!input.matches || !input.matches('input[id^="new_user_pass-"]')) return;
  let meter = input.parentElement.querySelector(".ds-pwd-strength");
  if (!meter) {
    input.insertAdjacentHTML("afterend", _pwdStrengthMarkup());
    meter = input.parentElement.querySelector(".ds-pwd-strength");
  }
  _updatePwdStrengthMeter(meter, input.value);
});

// El modal de cambio de contraseña (`promptModal`, admin-ui.js) es un único
// `#ds-prompt-modal` reutilizado por varios flujos (renombrar palabra clave,
// comentario de versión...); el medidor se engancha y se retira sólo mientras
// dura este diálogo concreto, para no aparecer en los demás usos de promptModal.
function _wirePwdStrengthOnPromptModal() {
  const modalEl = document.getElementById("ds-prompt-modal");
  const input = modalEl?.querySelector(".ds-pm-input");
  if (!input) return () => {};
  let meter = modalEl.querySelector(".ds-pwd-strength");
  if (!meter) {
    input.insertAdjacentHTML("afterend", _pwdStrengthMarkup());
    meter = input.nextElementSibling;
  }
  _updatePwdStrengthMeter(meter, input.value);
  const onInput = () => _updatePwdStrengthMeter(meter, input.value);
  input.addEventListener("input", onInput);
  return () => {
    input.removeEventListener("input", onInput);
    meter?.remove();
  };
}

async function handleChangePassword(uid, username) {
  const usaPromptModal = typeof promptModal === "function";
  const passPromise = usaPromptModal
    ? promptModal(`Cambiar contraseña de "${username}"`, "Nueva contraseña (mín. 6 caracteres)", "", "Nueva contraseña...", "password")
    : Promise.resolve(prompt(`Nueva contraseña para "${username}" (mín. 6 caracteres):`));
  const cleanupMeter = usaPromptModal ? _wirePwdStrengthOnPromptModal() : () => {};
  const newPass = await passPromise;
  cleanupMeter();
  if (!newPass || newPass.trim().length < 6) {
    if (newPass !== null) showToast("La contraseña debe tener al menos 6 caracteres.", "warning");
    return;
  }
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/users/${uid}/password`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ new_password: newPass.trim(), requester: state.user.username }),
    });
    showToast(`Contraseña de "${username}" actualizada.`, "success");
  } catch {
    showToast("Error al cambiar la contraseña.", "error");
  }
}

// OA-166: no hay una vista "qué se le ha hecho a esta cuenta" — hasta que
// haya una pestaña dedicada, reutilizamos el buscador de auditoría filtrando
// por el nombre de usuario, que es lo que hoy se pregunta primero.
function handleViewUserHistory(username) {
  if (typeof loadAdminTab === "function") loadAdminTab("audit");
  const suf = adminSuffixFromTab();
  const searchInput = document.getElementById(`audit_search-${suf}`);
  if (searchInput) {
    searchInput.value = username;
    auditState.page = 1;
    loadAuditTab();
  }
  showToast(`Mostrando auditoría de "${username}".`, "info");
}

async function loadAuditTab() {
  const suf    = adminSuffixFromTab();
  const body   = document.getElementById(`audit_table_body-${suf}`);
  const search = document.getElementById(`audit_search-${suf}`)?.value || "";
  if (!body) return;
  // OA-158: sin estado de carga la tabla se quedaba con los datos anteriores
  // hasta que llegaba la respuesta, y al teclear en el buscador parpadeaba
  // de "resultado viejo" a "resultado nuevo" sin ningún aviso intermedio.
  body.closest("table")?.setAttribute("aria-busy", "true");
  if (typeof showTableSkeleton === "function") showTableSkeleton(`audit_table_body-${suf}`, 6, 5);
  try {
    const url = `${API_BASE}/api/admin/audit_log?page=${auditState.page}&per_page=${auditState.perPage}&search=${encodeURIComponent(search)}`;
    const data = await apiFetchJSON(url);
    auditState.total = data.total;

    const totalPages = Math.ceil(data.total / auditState.perPage) || 1;
    const summaryEl  = document.getElementById(`audit_summary-${suf}`);
    const pageInfoEl = document.getElementById(`audit_page_info-${suf}`);
    const prevBtn    = document.getElementById(`audit_prev-${suf}`);
    const nextBtn    = document.getElementById(`audit_next-${suf}`);
    if (summaryEl)  summaryEl.innerText  = `${data.total} eventos registrados`;
    if (pageInfoEl) pageInfoEl.innerText = `Pág ${auditState.page} / ${totalPages}`;
    if (prevBtn)    prevBtn.disabled = auditState.page <= 1;
    if (nextBtn)    nextBtn.disabled = auditState.page >= totalPages;

    const colorResult = r => r === "Success" || r === "success" ? "text-success" : r === "Failure" ? "text-danger" : "text-muted";

    // OA-154: la celda se sigue recortando visualmente, pero ahora es un botón
    // real -alcanzable con teclado y con nombre accesible- que abre el detalle
    // completo en un modal, en vez de depender de que alguien pase el ratón
    // por encima del `title`.
    body.innerHTML = data.records.length === 0
      ? `<tr><td colspan="6" class="text-center text-muted p-3">Sin eventos registrados.</td></tr>`
      : data.records.map(r => {
          const detalle = r.detalle || "";
          // Sin inline onclick: el detalle es texto libre de auditoría y puede
          // traer comillas que romperían un atributo HTML construido a mano.
          const detalleCell = detalle
            ? `<button type="button" class="btn btn-link btn-sm p-0 text-muted text-left ds-audit-detalle-btn"
                       style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;display:block;"
                       data-evento="${escHtml(r.evento || "evento")}" data-detalle="${escHtml(detalle)}"
                       title="Ver detalle completo">${escHtml(detalle)}</button>`
            : "";
          return `
          <tr>
            <td class="text-muted">${escHtml(r.timestamp || "")}</td>
            <td class="font-weight-bold">${escHtml(r.usuario || "")}</td>
            <td>${escHtml(r.evento || "")}</td>
            <td><span class="badge badge-secondary">${escHtml(r.modulo || "")}</span></td>
            <td class="text-muted">${detalleCell}</td>
            <td class="${colorResult(r.resultado)}">${escHtml(r.resultado || "OK")}</td>
          </tr>`;
        }).join("");
  } catch (e) {
    if (body) body.innerHTML = `<tr><td colspan="6" class="text-danger text-center p-3">Error cargando auditoría.</td></tr>`;
  } finally {
    body.closest("table")?.removeAttribute("aria-busy");
  }
}

document.addEventListener("click", e => {
  const btn = e.target.closest(".ds-audit-detalle-btn");
  if (!btn) return;
  if (typeof detailModal === "function") {
    detailModal(`Detalle — ${btn.dataset.evento || "evento"}`, btn.dataset.detalle || "");
  }
});

function changeAuditPage(delta) {
  const totalPages = Math.ceil(auditState.total / auditState.perPage) || 1;
  auditState.page = Math.max(1, Math.min(auditState.page + delta, totalPages));
  loadAuditTab();
}

async function handleAddUser() {
  const suf      = adminSuffixFromTab();
  const username = document.getElementById(`new_user_name-${suf}`)?.value.trim() || "";
  const pass     = document.getElementById(`new_user_pass-${suf}`)?.value.trim()  || "";
  const modulo   = document.getElementById(`new_user_modulo-${suf}`)?.value       || "";
  const rol      = document.getElementById(`new_user_rol-${suf}`)?.value          || "";
  if (!username || !pass) { showToast("Por favor, ingrese todos los datos requeridos.", "warning"); return; }
  try {
    await apiFetchJSON(`${API_BASE}/api/admin/users/create`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ usuario: username, password: pass, modulo, rol, creator: state.user.username })
    });
    showToast(`¡Usuario ${username} registrado con éxito!`, "success");
    document.getElementById(`new_user_name-${suf}`) && (document.getElementById(`new_user_name-${suf}`).value = "");
    document.getElementById(`new_user_pass-${suf}`) && (document.getElementById(`new_user_pass-${suf}`).value = "");
    loadUsersTab();
  } catch (err) {
    showToast(err.message || "Error al registrar el nuevo usuario.", "error");
  }
}
