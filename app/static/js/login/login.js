const API_BASE   = window.location.origin;
const SESSION_KEY = "archive_session";
const LOCK_KEY = "login_lock_until";
let _loginFailCount = 0;
let _locked = false;
let _lockTimer = null;

document.addEventListener("DOMContentLoaded", () => {
  initLoginPage();
  checkExistingSession();
});

// BUG-login-no-lee-sesion: login.html no carga app.js (ver comentario
// VI-075/SI-049 más arriba en login.html), así que checkPersistedSession()
// de app.js nunca se ejecuta aquí — quien ya tenía sesión activa y volvía a
// /login se quedaba viendo el formulario en vez de ser redirigido. Réplica
// mínima de esa lógica (mismo TTL de 12h y mismo endpoint de validación)
// sin necesidad de cargar app.js entero en esta página.
function checkExistingSession() {
  const raw = localStorage.getItem(SESSION_KEY);
  if (!raw) return;
  let saved;
  try {
    saved = JSON.parse(raw);
  } catch {
    localStorage.removeItem(SESSION_KEY);
    return;
  }
  const ttlMs = 12 * 60 * 60 * 1000;
  if (!saved || !saved.username || !saved.ts || (Date.now() - saved.ts) >= ttlMs) {
    localStorage.removeItem(SESSION_KEY);
    return;
  }
  fetch(`${API_BASE}/api/auth/restore`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ username: saved.username })
  })
    .then(res => {
      if (!res.ok) {
        localStorage.removeItem(SESSION_KEY);
        return;
      }
      return res.json().then(data => {
        window.location.href = chooseLandingPage(data.user || saved);
      });
    })
    .catch(() => {
      // Error de red: no expulsar de la sesión guardada, simplemente
      // dejar el formulario visible como si no hubiéramos comprobado nada.
    });
}

function initLoginPage() {
  const toggle   = document.getElementById("toggle_login_pass");
  const loginForm = document.getElementById("login_form");
  const loginBtn = document.getElementById("login_btn");
  const passInput = document.getElementById("login_pass");
  const userInput = document.getElementById("login_user");

  toggle?.addEventListener("click", () => {
    if (!passInput) return;
    passInput.type = passInput.type === "password" ? "text" : "password";
    const showing = passInput.type === "text";
    toggle.innerHTML = showing
      ? '<i class="fas fa-eye"></i>'
      : '<i class="fas fa-eye-slash"></i>';
    toggle.setAttribute("aria-pressed", String(showing));
    toggle.setAttribute("aria-label", showing ? "Ocultar contraseña" : "Mostrar contraseña");
  });

  const updateCapsWarning = e => {
    const warn = document.getElementById("caps-lock-warning");
    if (warn) warn.style.display = e.getModifierState?.("CapsLock") ? "block" : "none";
  };
  const hideCapsWarning = () => {
    const warn = document.getElementById("caps-lock-warning");
    if (warn) warn.style.display = "none";
  };
  [userInput, passInput].forEach(el => {
    el?.addEventListener("keydown", updateCapsWarning);
    el?.addEventListener("keyup", updateCapsWarning);
    el?.addEventListener("blur", hideCapsWarning);
  });

  if (loginForm) {
    loginForm.addEventListener("submit", (e) => {
      e.preventDefault();
      performLogin();
    });
  } else {
    loginBtn?.addEventListener("click", performLogin);
    document.addEventListener("keydown", (e) => {
      if (e.key === "Enter") performLogin();
    });
  }

  restoreLockFromStorage();
}

function restoreLockFromStorage() {
  const until = Number(localStorage.getItem(LOCK_KEY) || 0);
  if (until > Date.now()) {
    lockLogin(until - Date.now());
  } else if (until) {
    localStorage.removeItem(LOCK_KEY);
  }
}

function lockLogin(durationMs) {
  const btn = document.getElementById("login_btn");
  _locked = true;
  const until = Date.now() + durationMs;
  localStorage.setItem(LOCK_KEY, String(until));
  if (btn) {
    if (!btn.dataset.origHtml) btn.dataset.origHtml = btn.innerHTML;
    btn.disabled = true;
    btn.setAttribute("aria-disabled", "true");
    const seconds = Math.ceil(durationMs / 1000);
    btn.innerHTML = `<i class="fas fa-lock mr-1"></i> Bloqueado (${seconds}s)`;
  }
  if (_lockTimer) clearTimeout(_lockTimer);
  _lockTimer = setTimeout(() => {
    _locked = false;
    _loginFailCount = 0;
    localStorage.removeItem(LOCK_KEY);
    if (btn) {
      btn.disabled = false;
      btn.removeAttribute("aria-disabled");
      btn.innerHTML = btn.dataset.origHtml || btn.innerHTML;
    }
    hideLoginError();
  }, durationMs);
}

function showLoginError(msg) {
  const errEl = document.getElementById("login-error-msg");
  if (errEl) {
    errEl.textContent = msg;
    errEl.style.display = "block";
    errEl.style.animation = "none";
    errEl.offsetHeight;
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
  if (_locked) return;

  const username = document.getElementById("login_user")?.value.trim();
  const password = document.getElementById("login_pass")?.value;
  if (!username || !password) {
    showLoginError("Ingrese su usuario y contraseña.");
    return;
  }
  hideLoginError();
  const btn = document.getElementById("login_btn");
  const origHTML = btn.innerHTML;
  if (!btn.dataset.origHtml) btn.dataset.origHtml = origHTML;
  btn.disabled = true;
  btn.setAttribute("aria-busy", "true");
  btn.innerHTML = '<span class="spinner-border spinner-border-sm mr-2" role="status" aria-hidden="true"></span> Verificando...';
  try {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username, password })
    });
    if (!res.ok) {
      const err = new Error("Usuario o contraseña incorrectos.");
      err.isAuthError = true;
      throw err;
    }
    const data = await res.json();
    saveSession(data.user);
    window.location.href = chooseLandingPage(data.user);
  } catch (e) {
    _loginFailCount++;
    let errMsg = e.isAuthError ? e.message : "Error de conexión. Intente nuevamente.";
    if (_loginFailCount >= 3) {
      errMsg += ` (Intento ${_loginFailCount} — verifique sus credenciales)`;
    }
    showLoginError(errMsg);
    btn.removeAttribute("aria-busy");
    if (_loginFailCount >= 5) {
      lockLogin(30000);
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
  if (user.modules?.includes("Archivo") && user.modules?.includes("RRHH")) return "/admin/sistema";
  if (user.modulo === "RRHH" || user.modules?.includes("RRHH")) return "/rrhh";
  return "/archivo";
}
