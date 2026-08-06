const API_BASE   = window.location.origin;
const SESSION_KEY = "archive_session";
let _loginFailCount = 0;

document.addEventListener("DOMContentLoaded", () => {
  initLoginPage();
});

function initLoginPage() {
  const toggle   = document.getElementById("toggle_login_pass");
  const loginBtn = document.getElementById("login_btn");
  const passInput = document.getElementById("login_pass");

  toggle?.addEventListener("click", () => {
    if (!passInput) return;
    passInput.type = passInput.type === "password" ? "text" : "password";
    toggle.innerHTML = passInput.type === "password"
      ? '<i class="fas fa-eye-slash"></i>'
      : '<i class="fas fa-eye"></i>';
  });

  // CapsLock indicator
  passInput?.addEventListener("keyup", e => {
    const warn = document.getElementById("caps-lock-warning");
    if (warn) warn.style.display = e.getModifierState?.("CapsLock") ? "block" : "none";
  });

  loginBtn?.addEventListener("click", performLogin);

  document.addEventListener("keydown", (e) => {
    if (e.key === "Enter") performLogin();
  });
}

function showLoginError(msg) {
  const errEl = document.getElementById("login-error-msg");
  if (errEl) {
    errEl.textContent = msg;
    errEl.style.display = "block";
    errEl.style.animation = "none";
    errEl.offsetHeight; // trigger reflow
    errEl.style.animation = "ds-shake 0.4s ease";
  } else {
    alert(msg);
  }
}

function hideLoginError() {
  const errEl = document.getElementById("login-error-msg");
  if (errEl) errEl.style.display = "none";
}

async function performLogin() {
  const username = document.getElementById("login_user")?.value.trim();
  const password = document.getElementById("login_pass")?.value;
  if (!username || !password) {
    showLoginError("Ingrese su usuario y contraseña.");
    return;
  }
  hideLoginError();
  const btn = document.getElementById("login_btn");
  const origHTML = btn.innerHTML;
  btn.disabled = true;
  btn.innerHTML = '<span class="spinner-border spinner-border-sm mr-2" role="status" aria-hidden="true"></span> Verificando...';
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    if (!res.ok) {
      const errData = await res.json().catch(() => ({}));
      throw new Error(errData.detail || "Credenciales incorrectas.");
    }
    const data = await res.json();
    saveSession(data.user);
    window.location.href = chooseLandingPage(data.user);
  } catch (e) {
    _loginFailCount++;
    let errMsg = e.message || "Error de conexión. Intente nuevamente.";
    if (_loginFailCount >= 3) {
      errMsg += ` (Intento ${_loginFailCount} — verifique sus credenciales)`;
    }
    showLoginError(errMsg);
    if (_loginFailCount >= 5) {
      btn.disabled = true;
      btn.innerHTML = '<i class="fas fa-lock mr-1"></i> Bloqueado (30s)';
      setTimeout(() => {
        btn.disabled = false;
        btn.innerHTML = origHTML;
        _loginFailCount = 0;
        hideLoginError();
      }, 30000);
    } else {
      btn.disabled = false;
      btn.innerHTML = origHTML;
    }
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
  // Global admin → sistema
  if (user.modules?.includes("Archivo") && user.modules?.includes("RRHH")) return "/admin/sistema";
  if (user.modulo === "RRHH" || user.modules?.includes("RRHH")) return "/rrhh";
  return "/archivo";
}
