const API_BASE   = window.location.origin;
const SESSION_KEY = "archive_session";
const LOCK_KEY = "login_lock_until";
let _loginFailCount = 0;
// Única fuente de verdad del bloqueo: mientras esté en `true` NINGÚN camino de
// envío (clic, Enter, submit del formulario) puede llamar a performLogin().
// SI-032: antes el bloqueo sólo deshabilitaba el botón y un listener de
// `keydown` en `document` llamaba a performLogin() sin mirar ese estado.
let _locked = false;
let _lockTimer = null;

document.addEventListener("DOMContentLoaded", () => {
  initLoginPage();
});

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

  // CapsLock indicator: reacciona a que se escriba o se pegue en cualquiera
  // de los dos campos, no sólo a `keyup` sobre la contraseña, y se oculta al
  // salir del campo.
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

  // Un solo camino de envío: el submit del formulario. Cubre el clic en el
  // botón (type="submit") y Enter en cualquier campo del formulario, así que
  // no puede haber un camino que se olvide de comprobar el bloqueo.
  if (loginForm) {
    loginForm.addEventListener("submit", (e) => {
      e.preventDefault();
      performLogin();
    });
  } else {
    // Red de seguridad si el marcado no trae <form>.
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
  // Cerrojo único: si está bloqueado, ningún camino de envío hace la
  // petición, sin importar cómo se haya disparado performLogin().
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
      // Mensaje genérico siempre, sin importar el detalle que devuelva el
      // servidor: no debe distinguir usuario inexistente de contraseña
      // incorrecta ni de cuenta desactivada.
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
  // Global admin → sistema
  if (user.modules?.includes("Archivo") && user.modules?.includes("RRHH")) return "/admin/sistema";
  if (user.modulo === "RRHH" || user.modules?.includes("RRHH")) return "/rrhh";
  return "/archivo";
}
