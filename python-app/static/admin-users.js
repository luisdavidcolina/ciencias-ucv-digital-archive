// =============================================================================
// ADMIN — Gestión de Usuarios y Auditoría
// Depende de: admin.js (state, API_BASE, adminSuffixFromTab, showToast)
// =============================================================================

let auditState = { page: 1, perPage: 50, total: 0 };

async function loadUsersTab() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`admin_users_table-${suf}`);
  try {
    const res = await fetch(`${API_BASE}/api/admin/users`);
    if (!res.ok) throw new Error();
    const users = await res.json();
    container.innerHTML = `
      <table class="table table-striped table-bordered" style="font-size:0.85rem;">
        <thead><tr class="bg-light"><th>Usuario</th><th>Contraseña</th><th>Módulo</th><th>Rol</th><th>Estado</th><th>Último Acceso</th><th>Acciones</th></tr></thead>
        <tbody>
          ${users.map(u => {
            const isActive  = u.is_active !== false;
            const lastLogin = u.last_login ? u.last_login.substring(0, 16).replace('T', ' ') : 'Nunca';
            return `
            <tr>
              <td class="font-weight-bold text-dark"><i class="fas fa-user-circle mr-1 text-secondary"></i> ${u.usuario}</td>
              <td class="text-muted">${u.password}</td>
              <td>${u.modulo}</td>
              <td><span class="badge ${u.rol === "Admin" ? "badge-danger" : "badge-primary"}">${u.rol}</span></td>
              <td>
                <button class="btn btn-xs ${isActive ? 'btn-success' : 'btn-secondary'} mr-1"
                        onclick="handleToggleUserActive(${u.id}, '${u.usuario}')"
                        title="${isActive ? 'Desactivar usuario' : 'Activar usuario'}">
                  <i class="fas fa-${isActive ? 'check-circle' : 'times-circle'}"></i>
                  ${isActive ? 'Activo' : 'Inactivo'}
                </button>
              </td>
              <td class="text-muted small">${lastLogin}</td>
              <td>
                <button class="btn btn-xs btn-outline-secondary" onclick="handleChangePassword(${u.id},'${u.usuario}')">
                  <i class="fas fa-key"></i> Cambiar clave
                </button>
              </td>
            </tr>`;
          }).join("")}
        </tbody>
      </table>
    `;
  } catch {
    container.innerHTML = `<div class="alert alert-danger">Error al cargar listado de seguridad.</div>`;
  }
}

async function handleToggleUserActive(uid, username) {
  try {
    const res = await fetch(`${API_BASE}/api/admin/users/${uid}/active?requester=${encodeURIComponent(state.user.username)}`, {
      method: "PATCH",
    });
    if (!res.ok) throw new Error();
    const data = await res.json();
    showToast(`Usuario "${username}" ${data.is_active ? 'activado' : 'desactivado'}.`, data.is_active ? "success" : "warning");
    loadUsersTab();
  } catch {
    showToast("Error al cambiar el estado del usuario.", "error");
  }
}

async function handleChangePassword(uid, username) {
  const newPass = typeof promptModal === "function"
    ? await promptModal(`Cambiar contraseña de "${username}"`, "Nueva contraseña (mín. 6 caracteres)", "", "Nueva contraseña...")
    : prompt(`Nueva contraseña para "${username}" (mín. 6 caracteres):`);
  if (!newPass || newPass.trim().length < 6) {
    if (newPass !== null) showToast("La contraseña debe tener al menos 6 caracteres.", "warning");
    return;
  }
  try {
    const res = await fetch(`${API_BASE}/api/admin/users/${uid}/password`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ new_password: newPass.trim(), requester: state.user.username }),
    });
    if (!res.ok) throw new Error();
    showToast(`Contraseña de "${username}" actualizada.`, "success");
  } catch {
    showToast("Error al cambiar la contraseña.", "error");
  }
}

async function loadAuditTab() {
  const suf    = adminSuffixFromTab();
  const body   = document.getElementById(`audit_table_body-${suf}`);
  const search = document.getElementById(`audit_search-${suf}`)?.value || "";
  if (!body) return;
  try {
    const url = `${API_BASE}/api/admin/audit_log?page=${auditState.page}&per_page=${auditState.perPage}&search=${encodeURIComponent(search)}`;
    const res = await fetch(url);
    if (!res.ok) throw new Error();
    const data = await res.json();
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

    body.innerHTML = data.records.length === 0
      ? `<tr><td colspan="6" class="text-center text-muted p-3">Sin eventos registrados.</td></tr>`
      : data.records.map(r => `
          <tr>
            <td class="text-muted">${r.timestamp || ""}</td>
            <td class="font-weight-bold">${r.usuario || ""}</td>
            <td>${r.evento || ""}</td>
            <td><span class="badge badge-secondary">${r.modulo || ""}</span></td>
            <td class="text-muted" style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;" title="${(r.detalle||'').replace(/"/g,'&quot;')}">${r.detalle || ""}</td>
            <td class="${colorResult(r.resultado)}">${r.resultado || "OK"}</td>
          </tr>`).join("");
  } catch (e) {
    if (body) body.innerHTML = `<tr><td colspan="6" class="text-danger text-center p-3">Error cargando auditoría.</td></tr>`;
  }
}

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
    const res = await fetch(`${API_BASE}/api/admin/users/create`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ usuario: username, password: pass, modulo, rol, creator: state.user.username })
    });
    if (!res.ok) {
      const err = await res.json();
      throw new Error(err.detail || "Error");
    }
    showToast(`¡Usuario ${username} registrado con éxito!`, "success");
    document.getElementById(`new_user_name-${suf}`) && (document.getElementById(`new_user_name-${suf}`).value = "");
    document.getElementById(`new_user_pass-${suf}`) && (document.getElementById(`new_user_pass-${suf}`).value = "");
    loadUsersTab();
  } catch (err) {
    showToast(err.message || "Error al registrar el nuevo usuario.", "error");
  }
}
