/* Asistente IA — burbuja de chat flotante.
 *
 * Se auto-inyecta: basta con <script src="/static/ai-widget.js" defer></script> en la página.
 *
 * QUÉ HACE Y QUÉ NO
 * -----------------
 * No sabe nada del archivo ni de permisos. Manda el historial a /api/ia/chat y pinta lo que
 * vuelve. El perfil lo decide el servidor a partir de la cookie de sesión firmada: si este
 * archivo mintiera, no cambiaría nada. Lo que sí hace es OCULTAR lo que el usuario no puede
 * usar (el clip de adjuntar), para no ofrecer un botón que va a dar 403.
 *
 * EL HISTORIAL VIVE EN sessionStorage, NO EN localStorage
 * ------------------------------------------------------
 * Al cerrar la pestaña se pierde. Es a propósito: una conversación puede tener el nombre de
 * un empleado o el contenido de un expediente, y eso no se queda en un equipo compartido de
 * la Facultad. El registro para auditoría lo guarda el servidor.
 */
(function () {
  "use strict";

  var STORE = "ia_chat_hilo";
  var MAX_LOCAL = 40;

  var estado = {
    abierto: false, cargando: false, convId: null, mensajes: [],
    perfil: "publico", puedeEscribir: false, propuestas: [], usuario: null
  };

  // --- persistencia del hilo (solo la pestaña actual) -----------------------

  function guardar() {
    try {
      sessionStorage.setItem(STORE, JSON.stringify({
        convId: estado.convId, mensajes: estado.mensajes.slice(-MAX_LOCAL)
      }));
    } catch (e) { /* modo privado o cuota llena: el chat sigue en memoria */ }
  }

  function restaurar() {
    try {
      var d = JSON.parse(sessionStorage.getItem(STORE) || "null");
      if (d && Array.isArray(d.mensajes)) {
        estado.mensajes = d.mensajes;
        estado.convId = d.convId || null;
      }
    } catch (e) { /* hilo corrupto: se empieza de cero */ }
  }

  // --- render --------------------------------------------------------------

  function esc(t) {
    return String(t == null ? "" : t)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  // El modelo responde en texto plano, pero suelta URLs de documentos digitalizados. Se
  // convierten en enlaces DESPUÉS de escapar, nunca antes: al revés, un título con HTML
  // dentro se ejecutaría en la página.
  //
  // A los `/api/files/<key>` hay que añadirles `?u=<usuario>`: ese endpoint lo exige además
  // de la cookie. Sin esto el enlace es correcto, el documento existe, y aun así da 401.
  function formatear(texto) {
    return esc(texto)
      .replace(/(https?:\/\/[^\s<]+[^\s<.,;:)\]}"']|\/api\/files\/[^\s<]+)/g, function (url) {
        var href = url;
        if (url.indexOf("/api/files/") === 0 && estado.usuario && url.indexOf("?u=") === -1) {
          href = url + "?u=" + encodeURIComponent(estado.usuario);
        }
        return '<a href="' + href + '" target="_blank" rel="noopener noreferrer">' + url + "</a>";
      })
      .replace(/\n/g, "<br>");
  }

  function pintar() {
    var caja = document.getElementById("ia-mensajes");
    if (!caja) return;

    if (!estado.mensajes.length) {
      var sugerencias = estado.puedeEscribir
        ? ['¿Qué documentos hay sobre el Consejo de Facultad?',
           'Corrige la fecha del documento 42 a 2019-03-15',
           '¿Qué documentos vencen en los próximos 60 días?']
        : ['¿Qué documentos hay sobre el Consejo de Facultad?',
           'Muéstrame las actas más recientes que estén digitalizadas',
           '¿Cuántos documentos tiene el archivo y de qué años?'];

      caja.innerHTML =
        '<div class="ia-vacio"><i class="fas fa-robot"></i>' +
        "<p><strong>Asistente del Archivo</strong></p>" +
        '<p class="ia-perfil">' + etiquetaPerfil() + "</p>" +
        '<p class="ia-sug-titulo">Prueba con:</p>' +
        sugerencias.map(function (s) {
          return '<button class="ia-sug">' + esc(s) + "</button>";
        }).join("") + "</div>";
      return;
    }

    var html = "";
    estado.mensajes.forEach(function (m) {
      if (m.rol === "sistema") {
        html += '<div class="ia-sistema">' + formatear(m.contenido) + "</div>";
        return;
      }
      html += '<div class="ia-msg ia-msg-' + (m.rol === "user" ? "user" : "bot") + '">' +
              formatear(m.contenido) + "</div>";
      if (m.herramientas && m.herramientas.length) {
        // Se muestra qué consultó. Sin esto, una respuesta correcta y una inventada se ven
        // exactamente igual, y el usuario no tiene cómo distinguirlas.
        html += '<div class="ia-traza"><i class="fas fa-database"></i> consultó: ' +
                esc(m.herramientas.map(function (h) { return h.herramienta; }).join(", ")) +
                "</div>";
      }
    });

    // Las propuestas pendientes: el único punto donde el asistente cambia algo, y siempre
    // con una persona apretando el botón.
    estado.propuestas.forEach(function (p) {
      html += '<div class="ia-propuesta" data-id="' + p.id + '">' +
        '<div class="ia-prop-cab"><i class="fas fa-pen-to-square"></i> Cambio propuesto</div>' +
        '<div class="ia-prop-txt">' + esc(p.resumen) + "</div>" +
        '<div class="ia-prop-btns">' +
          '<button class="ia-aprobar" data-id="' + p.id + '"><i class="fas fa-check"></i> Aprobar</button>' +
          '<button class="ia-rechazar" data-id="' + p.id + '"><i class="fas fa-times"></i> Rechazar</button>' +
        "</div></div>";
    });

    if (estado.cargando) {
      html += '<div class="ia-msg ia-msg-bot ia-escribiendo"><span></span><span></span><span></span></div>';
    }

    caja.innerHTML = html;
    caja.scrollTop = caja.scrollHeight;
  }

  function etiquetaPerfil() {
    if (estado.perfil === "editor") return "Puedes consultar y proponer cambios.";
    if (estado.perfil === "consulta") return "Modo consulta: puedes buscar, no modificar.";
    return "Consulta pública del Archivo Institucional.";
  }

  function sistema(texto) {
    estado.mensajes.push({ rol: "sistema", contenido: texto });
    pintar();
    guardar();
  }

  // --- envío ---------------------------------------------------------------

  function enviar(texto) {
    texto = (texto || "").trim();
    if (!texto || estado.cargando) return;

    estado.mensajes.push({ rol: "user", contenido: texto });
    estado.cargando = true;
    pintar();
    guardar();

    fetch("/api/ia/chat", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      // La sesión viaja en la cookie HttpOnly: no hay token que este script pueda leer,
      // y por eso mismo no hay token que un XSS pueda robarle.
      credentials: "same-origin",
      body: JSON.stringify({
        mensajes: estado.mensajes
          .filter(function (m) { return m.rol === "user" || m.rol === "assistant"; })
          .map(function (m) { return { rol: m.rol, contenido: m.contenido }; }),
        conversacion_id: estado.convId
      })
    })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        estado.cargando = false;
        if (!res.ok) {
          estado.mensajes.push({
            rol: "assistant",
            contenido: "⚠️ " + (res.d.detail || "No se pudo consultar al asistente.")
          });
        } else {
          estado.convId = res.d.conversacion_id || estado.convId;
          estado.perfil = res.d.perfil || estado.perfil;
          estado.propuestas = res.d.propuestas || [];
          estado.mensajes.push({
            rol: "assistant", contenido: res.d.respuesta, herramientas: res.d.herramientas
          });
          if (res.d.navegar_a) {
            setTimeout(function () { window.location.href = res.d.navegar_a; }, 1200);
          }
        }
        pintar();
        guardar();
      })
      .catch(function () {
        estado.cargando = false;
        estado.mensajes.push({ rol: "assistant", contenido: "⚠️ Error de red. Intenta de nuevo." });
        pintar();
      });
  }

  // --- historial de conversaciones -----------------------------------------
  //
  // Solo para usuarios con sesión: las del público no tienen dueño y no se pueden retomar
  // (el hilo vive en sessionStorage y muere con la pestaña, que es lo que queremos).

  function alternarHistorial() {
    var lat = document.getElementById("ia-historial");
    var abierto = lat.classList.toggle("ia-hist-abierto");
    if (abierto) cargarHistorial();
  }

  function cargarHistorial() {
    var lista = document.getElementById("ia-hist-lista");
    lista.innerHTML = '<div class="ia-hist-vacio">Cargando…</div>';

    fetch("/api/ia/conversaciones?limite=50", { credentials: "same-origin" })
      .then(function (r) { return r.ok ? r.json() : { conversaciones: [] }; })
      .then(function (d) {
        var convs = d.conversaciones || [];
        if (!convs.length) {
          lista.innerHTML = '<div class="ia-hist-vacio">Todavía no hay conversaciones.</div>';
          return;
        }
        lista.innerHTML = convs.map(function (c) {
          return '<div class="ia-hist-item' +
            (c.id === estado.convId ? " ia-hist-activa" : "") + '" data-id="' + c.id + '">' +
            '<div class="ia-hist-titulo">' + esc(c.titulo || "(sin título)") + "</div>" +
            '<div class="ia-hist-meta">' + esc(c.ultima || "") + " · " + c.mensajes + " msgs</div>" +
            '<button class="ia-hist-borrar" data-id="' + c.id +
              '" title="Borrar"><i class="fas fa-trash"></i></button>' +
            "</div>";
        }).join("");
      })
      .catch(function () {
        lista.innerHTML = '<div class="ia-hist-vacio">No se pudo cargar el historial.</div>';
      });
  }

  function restaurarConversacion(id) {
    fetch("/api/ia/conversacion/" + id, { credentials: "same-origin" })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        if (!res.ok) { sistema("⚠️ " + (res.d.detail || "No se pudo abrir esa conversación.")); return; }
        estado.convId = id;
        estado.propuestas = res.d.propuestas || [];
        estado.mensajes = (res.d.mensajes || []).map(function (m) {
          return { rol: m.rol, contenido: m.contenido, herramientas: m.herramientas };
        });
        document.getElementById("ia-historial").classList.remove("ia-hist-abierto");
        pintar();
        guardar();
      })
      .catch(function () { sistema("⚠️ Error de red al abrir la conversación."); });
  }

  function borrarConversacion(id) {
    if (!window.confirm("¿Borrar esta conversación y todos sus mensajes?")) return;
    fetch("/api/ia/conversacion/" + id, { method: "DELETE", credentials: "same-origin" })
      .then(function (r) {
        if (!r.ok) { sistema("⚠️ No se pudo borrar la conversación."); return; }
        // Si borraste la que tenías abierta, el chat se queda sin hilo: hay que empezar uno
        // nuevo, no seguir escribiendo contra un id que ya no existe.
        if (id === estado.convId) limpiar();
        cargarHistorial();
      })
      .catch(function () { sistema("⚠️ Error de red al borrar."); });
  }

  // --- propuestas ----------------------------------------------------------

  function resolver(id, accion) {
    var fila = document.querySelector('.ia-propuesta[data-id="' + id + '"]');
    if (fila) fila.classList.add("ia-prop-ocupada");

    fetch("/api/ia/propuesta/" + id + "/" + accion, {
      method: "POST", credentials: "same-origin"
    })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        estado.propuestas = estado.propuestas.filter(function (p) { return p.id !== id; });
        if (!res.ok) {
          sistema("⚠️ " + (res.d.detail || "No se pudo aplicar el cambio."));
        } else if (accion === "aprobar") {
          sistema("✅ Cambio aplicado. " + (res.d.detalle || ""));
        } else {
          sistema("Propuesta descartada.");
        }
      })
      .catch(function () { sistema("⚠️ Error de red al resolver la propuesta."); });
  }

  // --- adjuntos ------------------------------------------------------------

  function subir(archivo) {
    if (!archivo) return;
    if (!estado.convId) {
      sistema("Escribe algo primero: el archivo se adjunta a una conversación.");
      return;
    }

    var fd = new FormData();
    fd.append("file", archivo);
    fd.append("conversacion_id", estado.convId);
    sistema("Subiendo «" + esc(archivo.name) + "»…");

    fetch("/api/ia/adjuntar", { method: "POST", credentials: "same-origin", body: fd })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        if (!res.ok) {
          sistema("⚠️ " + (res.d.detail || "No se pudo subir el archivo."));
          return;
        }
        sistema("📎 «" + esc(res.d.nombre_archivo) + "» subido.");
        // Se le avisa al modelo con un turno de usuario: es la forma de que lo sepa sin
        // inventar un canal aparte, y deja el hecho escrito en la conversación.
        enviar("Acabo de subir el archivo «" + res.d.nombre_archivo +
               "». Revísalo con mis_adjuntos y dime a qué documento lo engancho.");
      })
      .catch(function () { sistema("⚠️ Error de red al subir el archivo."); });
  }

  // --- montaje -------------------------------------------------------------

  function alternar() {
    estado.abierto = !estado.abierto;
    document.getElementById("ia-panel").classList.toggle("ia-abierto", estado.abierto);
    if (estado.abierto) {
      pintar();
      var i = document.getElementById("ia-input");
      if (i) i.focus();
    }
  }

  function limpiar() {
    estado.mensajes = [];
    estado.convId = null;
    estado.propuestas = [];
    guardar();
    pintar();
  }

  function montar() {
    if (document.getElementById("ia-burbuja")) return;

    var cont = document.createElement("div");
    cont.innerHTML =
      '<button id="ia-burbuja" title="Asistente del Archivo" aria-label="Abrir asistente">' +
        '<i class="fas fa-robot"></i></button>' +
      '<div id="ia-panel" role="dialog" aria-label="Asistente del Archivo">' +
        '<div class="ia-cab">' +
          '<span><i class="fas fa-robot"></i> Asistente del Archivo</span>' +
          "<div>" +
            '<button id="ia-hist-btn" title="Conversaciones anteriores" style="display:none">' +
              '<i class="fas fa-clock-rotate-left"></i></button>' +
            '<button id="ia-limpiar" title="Nueva conversación"><i class="fas fa-plus"></i></button>' +
            '<button id="ia-cerrar" title="Cerrar"><i class="fas fa-times"></i></button>' +
          "</div></div>" +
        '<div id="ia-aviso"></div>' +
        '<div id="ia-historial">' +
          '<div class="ia-hist-cab">Conversaciones anteriores</div>' +
          '<div id="ia-hist-lista"></div>' +
        "</div>" +
        '<div id="ia-mensajes"></div>' +
        '<form id="ia-form">' +
          '<button type="button" id="ia-clip" title="Adjuntar archivo" style="display:none">' +
            '<i class="fas fa-paperclip"></i></button>' +
          '<input id="ia-file" type="file" style="display:none">' +
          '<input id="ia-input" type="text" maxlength="2000" autocomplete="off" ' +
                 'placeholder="Pregunta sobre los documentos…">' +
          '<button type="submit" aria-label="Enviar"><i class="fas fa-paper-plane"></i></button>' +
        "</form></div>";
    document.body.appendChild(cont);

    document.getElementById("ia-burbuja").addEventListener("click", alternar);
    document.getElementById("ia-cerrar").addEventListener("click", alternar);
    document.getElementById("ia-limpiar").addEventListener("click", limpiar);
    document.getElementById("ia-hist-btn").addEventListener("click", alternarHistorial);

    document.getElementById("ia-hist-lista").addEventListener("click", function (e) {
      var bor = e.target.closest(".ia-hist-borrar");
      if (bor) { borrarConversacion(Number(bor.dataset.id)); return; }
      var item = e.target.closest(".ia-hist-item");
      if (item) restaurarConversacion(Number(item.dataset.id));
    });

    document.getElementById("ia-form").addEventListener("submit", function (e) {
      e.preventDefault();
      var i = document.getElementById("ia-input");
      enviar(i.value);
      i.value = "";
    });

    document.getElementById("ia-clip").addEventListener("click", function () {
      document.getElementById("ia-file").click();
    });
    document.getElementById("ia-file").addEventListener("change", function (e) {
      subir(e.target.files[0]);
      e.target.value = "";
    });

    document.getElementById("ia-mensajes").addEventListener("click", function (e) {
      var sug = e.target.closest(".ia-sug");
      if (sug) { enviar(sug.textContent); return; }
      var ap = e.target.closest(".ia-aprobar");
      if (ap) { resolver(Number(ap.dataset.id), "aprobar"); return; }
      var re = e.target.closest(".ia-rechazar");
      if (re) { resolver(Number(re.dataset.id), "rechazar"); }
    });

    // Si falta la clave o está deshabilitado, no se muestra la burbuja: mejor que no exista
    // a que exista y falle en cada clic.
    fetch("/api/ia/disponible", { credentials: "same-origin" })
      .then(function (r) { return r.json(); })
      .then(function (d) {
        if (!d.disponible) {
          document.getElementById("ia-burbuja").style.display = "none";
          return;
        }
        estado.perfil = d.perfil || "publico";
        estado.puedeEscribir = !!d.puede_escribir;
        estado.usuario = d.usuario || null;
        if (estado.puedeEscribir) {
          document.getElementById("ia-clip").style.display = "";
        }
        // El historial solo existe con sesión: las conversaciones del público no tienen
        // dueño y no se pueden retomar, a propósito.
        if (estado.usuario) {
          document.getElementById("ia-hist-btn").style.display = "";
        }
        // Cuando el gasto del día pasa el 80% del tope, se avisa antes de que corte.
        if (d.tope_diario && d.gasto_hoy >= d.tope_diario * 0.8) {
          var av = document.getElementById("ia-aviso");
          av.textContent = "Gasto de hoy: $" + Number(d.gasto_hoy).toFixed(4) +
                           " de $" + Number(d.tope_diario).toFixed(2);
          av.style.display = "block";
        }
        pintar();
      })
      .catch(function () {
        document.getElementById("ia-burbuja").style.display = "none";
      });

    restaurar();
    pintar();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", montar);
  } else {
    montar();
  }
})();
