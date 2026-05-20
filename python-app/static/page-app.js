const API_BASE = window.location.origin;
const SESSION_KEY = "archive_session";
const page = document.body.dataset.page || "";

document.addEventListener("DOMContentLoaded", () => {
  if (page === "login") {
    initLoginPage();
    return;
  }
  const session = getSession();
  if (!session) {
    window.location.href = "/";
    return;
  }
  setUserInfo(session);
  if (page === "archivo") initArchivoPage();
  else if (page === "rrhh") initRrhhPage();
  else if (page === "admin_archivo") initAdminPage("Archivo");
  else if (page === "admin_rrhh") initAdminPage("RRHH");
});

function getSession() {
  try {
    const raw = localStorage.getItem(SESSION_KEY);
    if (!raw) return null;
    const parsed = JSON.parse(raw);
    if (!parsed.username) return null;
    return parsed;
  } catch (error) {
    return null;
  }
}

function saveSession(user) {
  const payload = {
    username: user.username,
    modules: user.modules || [user.modulo],
    roles: user.roles || { [user.modulo]: user.rol },
    modulo: user.modulo,
    rol: user.rol,
    ts: Date.now()
  };
  localStorage.setItem(SESSION_KEY, JSON.stringify(payload));
}

function clearSession() {
  localStorage.removeItem(SESSION_KEY);
}

function setUserInfo(session) {
  const badge = document.getElementById("current_user_info");
  if (badge) {
    badge.innerText = `${session.username} · ${session.modulo} (${session.roles[session.modulo] || session.rol})`;
  }
  const logoutBtn = document.getElementById("logout_btn");
  if (logoutBtn) {
    logoutBtn.addEventListener("click", () => {
      clearSession();
      window.location.href = "/";
    });
  }
}

function initLoginPage() {
  const toggle = document.getElementById("toggle_login_pass");
  const loginBtn = document.getElementById("login_btn");
  toggle?.addEventListener("click", () => {
    const pass = document.getElementById("login_pass");
    if (!pass) return;
    pass.type = pass.type === "password" ? "text" : "password";
    toggle.innerHTML = pass.type === "password" ? '<i class="fas fa-eye-slash"></i>' : '<i class="fas fa-eye"></i>';
  });
  loginBtn?.addEventListener("click", performLogin);
  document.getElementById("login_pass")?.addEventListener("keydown", (e) => {
    if (e.key === "Enter") performLogin();
  });
}

async function performLogin() {
  const username = document.getElementById("login_user")?.value.trim();
  const password = document.getElementById("login_pass")?.value.trim();
  if (!username || !password) {
    alert("Ingrese usuario y contraseña.");
    return;
  }
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    if (!res.ok) throw new Error("Credenciales inválidas");
    const data = await res.json();
    saveSession(data.user);
    const target = chooseLandingPage(data.user);
    window.location.href = target;
  } catch (err) {
    alert("Credenciales incorrectas o error de conexión.");
  }
}

function chooseLandingPage(user) {
  if (!user) return "/archivo";
  if (user.roles && user.roles.Archivo === "Admin") return "/admin/archivo";
  if (user.roles && user.roles.RRHH === "Admin") return "/admin/rrhh";
  if (user.modules?.includes("Archivo")) return "/archivo";
  if (user.modules?.includes("RRHH")) return "/rrhh";
  return "/archivo";
}

async function initArchivoPage() {
  bindSearchEvents(searchArchivo);
  await loadArchivoChoices();
  await searchArchivo();
}

async function initRrhhPage() {
  bindSearchEvents(searchRrhh);
  await loadRrhhChoices();
  await searchRrhh();
}

async function initAdminPage(modulo) {
  const heading = document.querySelector("main h3");
  if (heading) heading.innerText = modulo === "RRHH" ? "Administración RRHH" : "Administración Archivo";
  document.getElementById("admin_submit_form")?.addEventListener("submit", (e) => {
    e.preventDefault();
    handleAdminSubmit(modulo);
  });
  await loadAdminStats(modulo);
  await loadAdminHistory(modulo);
}

function bindSearchEvents(handler) {
  document.getElementById("search_button")?.addEventListener("click", handler);
  document.getElementById("search_term")?.addEventListener("keydown", (e) => {
    if (e.key === "Enter") handler();
  });
}

async function loadArchivoChoices() {
  try {
    const res = await fetch(`${API_BASE}/api/choices`);
    const data = await res.json();
    const selector = document.getElementById("doc_type_selector");
    if (!selector) return;
    selector.innerHTML = `<option value="">Todas</option>` + data.archivo.doc_types.map(t => `<option value="${t}">${t}</option>`).join("");
    document.getElementById("date_start").value = data.archivo.min_date;
    document.getElementById("date_end").value = data.archivo.max_date;
  } catch (err) {
    console.error(err);
  }
}

async function loadRrhhChoices() {
  try {
    const res = await fetch(`${API_BASE}/api/choices`);
    const data = await res.json();
    const docType = document.getElementById("doc_type_selector");
    const people = document.getElementById("people_selector");
    const estado = document.getElementById("estado_selector");
    if (docType) docType.innerHTML = `<option value="">Todas</option>` + data.rrhh.doc_types.map(t => `<option value="${t}">${t}</option>`).join("");
    if (people) people.innerHTML = `<option value="">Todas</option>` + data.rrhh.people.map(p => `<option value="${p}">${p}</option>`).join("");
    if (estado) estado.innerHTML = `<option value="">Todos</option>` + data.rrhh.estados.map(s => `<option value="${s}">${s}</option>`).join("");
  } catch (err) {
    console.error(err);
  }
}

async function searchArchivo() {
  const payload = {
    search_term: document.getElementById("search_term")?.value || "",
    doc_types: getMultiValue("doc_type_selector"),
    date_start: document.getElementById("date_start")?.value || "",
    date_end: document.getElementById("date_end")?.value || ""
  };
  renderLoading("archivo_results");
  try {
    const res = await fetch(`${API_BASE}/api/archivo/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    const data = await res.json();
    renderArchivoResults(data);
  } catch (err) {
    renderError("archivo_results", "Error de búsqueda.");
  }
}

async function searchRrhh() {
  const payload = {
    search_term: document.getElementById("search_term")?.value || "",
    doc_types: getMultiValue("doc_type_selector"),
    people_terms: getMultiValue("people_selector"),
    estados: getMultiValue("estado_selector")
  };
  renderLoading("archivo_results");
  try {
    const res = await fetch(`${API_BASE}/api/rrhh/buscar`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    const data = await res.json();
    renderRrhhResults(data);
  } catch (err) {
    renderError("archivo_results", "Error de búsqueda.");
  }
}

function getMultiValue(id) {
  const element = document.getElementById(id);
  if (!element) return [];
  if (element.multiple) return Array.from(element.selectedOptions).map(o => o.value).filter(v => v);
  return element.value ? [element.value] : [];
}

function renderLoading(targetId) {
  const target = document.getElementById(targetId);
  if (target) target.innerHTML = `<div class="p-4 text-center text-muted">Cargando...</div>`;
}

function renderError(targetId, message) {
  const target = document.getElementById(targetId);
  if (target) target.innerHTML = `<div class="p-4 text-danger">${message}</div>`;
}

function renderArchivoResults(items) {
  const target = document.getElementById("archivo_results");
  if (!target) return;
  if (!items || items.length === 0) {
    target.innerHTML = `<div class="p-4 text-muted">No se encontraron resultados.</div>`;
    return;
  }
  target.innerHTML = `
    <table class="table table-striped mb-0">
      <thead><tr><th>Título</th><th>Autor</th><th>Tipo</th><th>Fecha</th><th>Ubicación</th></tr></thead>
      <tbody>${items.map(item => `<tr><td>${item.titulo || item.empleado || "-"}</td><td>${item.autor || item.cedula || "-"}</td><td>${item.doc_type || "-"}</td><td>${item.fecha || item.fecha_ingreso || "-"}</td><td>${item.ubicacion || "-"}</td></tr>`).join("")}</tbody>
    </table>
  `;
}

function renderRrhhResults(items) {
  const target = document.getElementById("archivo_results");
  if (!target) return;
  if (!items || items.length === 0) {
    target.innerHTML = `<div class="p-4 text-muted">No se encontraron perfiles.</div>`;
    return;
  }
  target.innerHTML = `
    <table class="table table-striped mb-0">
      <thead><tr><th>Persona</th><th>Cédula</th><th>Departamento</th><th>Estado</th><th>Tipos</th></tr></thead>
      <tbody>${items.map(item => `<tr><td>${item.persona}</td><td>${item.cedulas}</td><td>${item.departamentos}</td><td>${item.estatuses}</td><td>${item.tipos}</td></tr>`).join("")}</tbody>
    </table>
  `;
}

async function handleAdminSubmit(modulo) {
  const payload = { modulo, usuario: getSession()?.username || "unknown", doc_type: "", fecha: "", ubicacion: "" };
  if (modulo === "Archivo") {
    payload.titulo = document.getElementById("reg_title")?.value || "";
    payload.autor = document.getElementById("reg_author")?.value || "";
    payload.doc_type = document.getElementById("reg_doc_type")?.value || "";
    payload.fecha = document.getElementById("reg_fecha")?.value || "";
    payload.ubicacion = document.getElementById("reg_location")?.value || "";
    payload.resumen = "";
  } else {
    payload.empleado = document.getElementById("reg_empleado")?.value || "";
    payload.cedula = document.getElementById("reg_cedula")?.value || "";
    payload.personas_relacionadas = payload.empleado;
    payload.departamento = document.getElementById("reg_departamento")?.value || "";
    payload.estado = document.getElementById("reg_estado")?.value || "";
    payload.doc_type = document.getElementById("reg_doc_type")?.value || "";
    payload.fecha = document.getElementById("reg_fecha")?.value || "";
    payload.ubicacion = document.getElementById("reg_location")?.value || "";
  }
  try {
    const res = await fetch(`${API_BASE}/api/admin/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    if (!res.ok) throw new Error();
    const data = await res.json();
    alert(`Registro creado con ID ${data.id}`);
    await loadAdminStats(modulo);
    await loadAdminHistory(modulo);
  } catch (err) {
    alert("No se pudo guardar el registro.");
  }
}

async function loadAdminStats(modulo) {
  try {
    const res = await fetch(`${API_BASE}/api/admin/stats`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ modulo, date_start: "", date_end: "" })
    });
    const data = await res.json();
    document.getElementById("stat_total_docs").innerText = data.total_docs;
    document.getElementById("stat_categories").innerText = data.categories_count;
    document.getElementById("stat_users").innerText = "--";
    document.getElementById("stat_latest").innerText = data.timeline?.length ? data.timeline[0].year : "N/A";
  } catch (err) {
    console.error(err);
  }
}

async function loadAdminHistory(modulo) {
  document.getElementById("admin_history").innerText = "Cargando...";
  try {
    const res = await fetch(`${API_BASE}/api/admin/list_all?modulo=${modulo}`);
    const data = await res.json();
    const history = data.slice(0, 6).map(item => {
      const title = modulo === "Archivo" ? item.titulo : item.empleado;
      return `<li>${title} — ${item.doc_type || item.departamento || "-"}</li>`;
    }).join("");
    document.getElementById("admin_history").innerHTML = `<ul class="pl-3">${history || "<li class=\"text-muted\">Sin registros recientes.</li>"}</ul>`;
  } catch (err) {
    document.getElementById("admin_history").innerText = "No se pudo cargar el historial.";
  }
}
