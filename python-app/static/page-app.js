const API_BASE   = window.location.origin;
const SESSION_KEY = "archive_session";

document.addEventListener("DOMContentLoaded", () => {
  initLoginPage();
});

function initLoginPage() {
  const toggle   = document.getElementById("toggle_login_pass");
  const loginBtn = document.getElementById("login_btn");

  toggle?.addEventListener("click", () => {
    const pass = document.getElementById("login_pass");
    if (!pass) return;
    pass.type = pass.type === "password" ? "text" : "password";
    toggle.innerHTML = pass.type === "password"
      ? '<i class="fas fa-eye-slash"></i>'
      : '<i class="fas fa-eye"></i>';
  });

  loginBtn?.addEventListener("click", performLogin);

  document.addEventListener("keydown", (e) => {
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
  const btn = document.getElementById("login_btn");
  btn.disabled = true;
  btn.innerHTML = '<span class="spinner-border spinner-border-sm mr-2" role="status" aria-hidden="true"></span> Verificando...';
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    if (!res.ok) throw new Error("Credenciales inválidas");
    const data = await res.json();
    saveSession(data.user);
    window.location.href = chooseLandingPage(data.user);
  } catch {
    alert("Credenciales incorrectas o error de conexión.");
    btn.disabled = false;
    btn.innerHTML = '<i class="fas fa-shield-alt" style="margin-right:8px;"></i> Autorizar Ingreso';
  }
}

function saveSession(user) {
  const payload = {
    username: user.username,
    modules:  user.modules || [user.modulo],
    roles:    user.roles   || { [user.modulo]: user.rol },
    modulo:   user.modulo,
    rol:      user.rol,
    ts:       Date.now()
  };
  localStorage.setItem(SESSION_KEY, JSON.stringify(payload));
}

function chooseLandingPage(user) {
  if (!user) return "/archivo";
  if (user.modulo === "RRHH" || user.modules?.includes("RRHH")) return "/rrhh";
  return "/archivo";
}
