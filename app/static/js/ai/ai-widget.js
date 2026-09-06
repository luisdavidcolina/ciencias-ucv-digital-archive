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
    perfil: "publico", puedeEscribir: false, propuestas: [], usuario: null,
    disponible: true
  };

  // SI-065: la llamada en curso, para poder cancelarla desde el botón «Detener». Sin
  // AbortController no había forma de cortar una petición que ya se estaba pagando.
  var peticionActual = null;

  // SI-111: sólo se repinta el mensaje nuevo cuando es posible; `renderCount` recuerda
  // cuántos ya están en el DOM para no reconstruir `#ia-mensajes` entero en cada turno,
  // que es lo que hacía que un lector de pantalla releyera la conversación completa.
  var renderCount = 0;

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

  // SI-118: `sessionStorage` no se comparte entre pestañas — salvo cuando una pestaña se
  // duplica, que arranca con una copia exacta del hilo (mismo `convId`). Si desde una se
  // borra la conversación, la otra sigue escribiendo contra un id que ya no existe, que es
  // justo el caso que `borrarConversacion()` ya cuida dentro de una sola pestaña. El evento
  // `storage` avisa cuando el navegador sí sincroniza esas pestañas, y al abrir el panel se
  // revalida contra el servidor por si no lo hizo.
  window.addEventListener("storage", function (e) {
    if (e.key !== STORE) return;
    restaurar();
    pintar();
  });

  function revalidarConvId() {
    if (!estado.convId) return;
    fetch("/api/ia/conversacion/" + estado.convId, { credentials: "same-origin" })
      .then(function (r) {
        if (r.status === 404) {
          sistema("Esta conversación se borró desde otra pestaña. Se empieza una nueva.");
          limpiar();
        }
      })
      .catch(function () { /* sin red: no se revalida, se sigue con lo que había */ });
  }

  // --- render --------------------------------------------------------------

  function esc(t) {
    return String(t == null ? "" : t)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  // SI-125: el nombre interno de la función no le dice nada a quien lee la traza.
  var NOMBRES_HERRAMIENTA = {
    buscar_archivo: "buscó en el Archivo",
    ver_documento: "abrió la ficha de un documento",
    listar_tipos_documento: "listó los tipos de documento",
    listar_palabras_clave: "listó palabras clave",
    estadisticas: "consultó las cifras generales",
    ir_a: "preparó un enlace de navegación",
    buscar_empleado: "buscó personal",
    expediente_empleado: "abrió un expediente de RRHH",
    buscar_documento_rrhh: "buscó documentos de RRHH",
    documentos_por_vencer: "revisó vencimientos",
    mis_adjuntos: "revisó los archivos adjuntos",
    proponer_actualizacion: "preparó una propuesta de cambio",
    proponer_documento: "preparó una propuesta de documento nuevo",
    proponer_adjuntar_archivo: "preparó una propuesta de adjunto",
    proponer_palabras_clave: "preparó una propuesta de palabras clave"
  };

  function nombreHerramienta(id) {
    return NOMBRES_HERRAMIENTA[id] || id;
  }

  // El modelo responde en texto plano, pero suelta URLs de documentos digitalizados. Se
  // convierten en enlaces DESPUÉS de escapar, nunca antes: al revés, un título con HTML
  // dentro se ejecutaría en la página.
  //
  // SI-017: solo se enlazan rutas propias (`/api/files/…`, `/compartido/…`). El texto de
  // los documentos (título, resumen, notas) lo escribe cualquier admin de módulo, y una
  // URL externa ahí dentro no lleva el sello de confianza de la institución detrás — se
  // deja como texto plano, no como enlace pulsable.
  //
  // A los `/api/files/<key>` hay que añadirles `?u=<usuario>`: ese endpoint lo exige además
  // de la cookie. Sin esto el enlace es correcto, el documento existe, y aun así da 401.
  // SI-120: el prompt le pide al modelo listar «uno por línea», y devuelve viñetas y
  // negritas en Markdown. Sin esto salían como asteriscos y guiones sueltos. Subconjunto
  // mínimo a propósito (negrita, listas, código en línea) y siempre DESPUÉS de `esc()`:
  // nunca se interpreta HTML del propio texto, sólo esta sintaxis reducida.
  function inline(s) {
    return s
      .replace(/`([^`]+)`/g, "<code>$1</code>")
      .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  }

  function markdownLigero(texto) {
    var lineas = texto.split("\n");
    var html = "", enLista = false;
    lineas.forEach(function (linea, i) {
      var item = /^\s*[-*]\s+(.*)$/.exec(linea);
      if (item) {
        if (!enLista) { html += "<ul>"; enLista = true; }
        html += "<li>" + inline(item[1]) + "</li>";
        return;
      }
      if (enLista) { html += "</ul>"; enLista = false; }
      html += inline(linea);
      if (i < lineas.length - 1) html += "<br>";
    });
    if (enLista) html += "</ul>";
    return html;
  }

  function formatear(texto) {
    var conEnlaces = esc(texto)
      .replace(/(\/api\/files\/[^\s<]+|\/compartido\/[^\s<]+)/g, function (url) {
        var href = url;
        if (url.indexOf("/api/files/") === 0 && estado.usuario && url.indexOf("?u=") === -1) {
          href = url + "?u=" + encodeURIComponent(estado.usuario);
        }
        return '<a href="' + href + '" target="_blank" rel="noopener noreferrer">' + url + "</a>";
      });
    return markdownLigero(conEnlaces);
  }

  function pintar() {
    var caja = document.getElementById("ia-mensajes");
    if (!caja) return;

    if (!estado.mensajes.length) {
      if (estado.disponible === false) {
        caja.innerHTML =
          '<div class="ia-vacio"><i class="fas fa-robot"></i>' +
          "<p><strong>Asistente del Archivo</strong></p>" +
          '<p class="ia-perfil">No se pudo conectar con el asistente. Vuelve a intentarlo ' +
          "más tarde.</p></div>";
        return;
      }
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

    // SI-111: sólo se pintan los mensajes que todavía no están en el DOM. Si la lista se
    // acortó (conversación nueva o restaurada), no hay nada que "aparecer": se reconstruye
    // entera y se reinicia el contador.
    if (renderCount > estado.mensajes.length) renderCount = 0;
    if (renderCount === 0) caja.innerHTML = "";

    var cola = document.getElementById("ia-cola");
    if (!cola) {
      cola = document.createElement("div");
      cola.id = "ia-cola";
      caja.appendChild(cola);
    }

    var html = "";
    for (var idx = renderCount; idx < estado.mensajes.length; idx++) {
      html += renderMensaje(estado.mensajes[idx], idx);
    }
    if (html) cola.insertAdjacentHTML("beforebegin", html);
    renderCount = estado.mensajes.length;

    // Las propuestas pendientes y el indicador de «escribiendo» se repintan aparte: cambian
    // sin que llegue un mensaje nuevo (se resuelven, o empieza/termina la espera), y así no
    // hace falta reconstruir la conversación entera para actualizarlos.
    var htmlCola = "";
    estado.propuestas.forEach(function (p) {
      htmlCola += '<div class="ia-propuesta" data-id="' + p.id + '">' +
        '<div class="ia-prop-cab"><i class="fas fa-pen-to-square"></i> Cambio propuesto</div>' +
        '<div class="ia-prop-txt">' + esc(p.resumen) + "</div>" +
        '<div class="ia-prop-btns">' +
          '<button class="ia-aprobar" data-id="' + p.id + '"><i class="fas fa-check"></i> Aprobar</button>' +
          '<button class="ia-rechazar" data-id="' + p.id + '"><i class="fas fa-times"></i> Rechazar</button>' +
        "</div></div>";
    });
    if (estado.cargando) {
      htmlCola += '<div class="ia-msg ia-msg-bot ia-escribiendo"><span></span><span></span><span></span></div>';
    }
    cola.innerHTML = htmlCola;

    caja.scrollTop = caja.scrollHeight;
  }

  function renderMensaje(m, idx) {
    if (m.rol === "sistema") {
      var html = '<div class="ia-sistema">' + formatear(m.contenido) + "</div>";
      // SI-116: subir un archivo ya no dispara un turno pago solo; se ofrece como
      // sugerencia pulsable, igual que las sugerencias iniciales.
      if (m.sugerencia) {
        html += '<button class="ia-sug" data-sugerencia="' + esc(m.sugerencia) +
                '">Preguntar por este archivo</button>';
      }
      // SI-117: el asistente ya no navega solo; ofrece el destino y decide la persona.
      if (m.navegar_a) {
        html += '<button class="ia-ir" data-href="' + esc(m.navegar_a) +
                '"><i class="fas fa-arrow-right"></i> Ir a la página</button>';
      }
      return html;
    }
    var out = '<div class="ia-msg ia-msg-' + (m.rol === "user" ? "user" : "bot") + '">' +
            formatear(m.contenido) +
            '<button class="ia-copiar" data-idx="' + idx +
            '" aria-label="Copiar mensaje" title="Copiar"><i class="fas fa-copy"></i></button>' +
            "</div>";
    if (m.herramientas && m.herramientas.length) {
      // Se muestra qué consultó. Sin esto, una respuesta correcta y una inventada se ven
      // exactamente igual, y el usuario no tiene cómo distinguirlas.
      out += '<div class="ia-traza"><i class="fas fa-database"></i> consultó: ' +
              esc(m.herramientas.map(function (h) { return nombreHerramienta(h.herramienta); }).join(", ")) +
              "</div>";
    }
    return out;
  }

  function etiquetaPerfil() {
    if (estado.perfil === "editor") return "Puedes consultar y proponer cambios.";
    if (estado.perfil === "consulta") return "Modo consulta: puedes buscar, no modificar.";
    return "Consulta pública del Archivo Institucional.";
  }

  function sistema(texto, extra) {
    var msg = { rol: "sistema", contenido: texto };
    if (extra) {
      for (var k in extra) { if (Object.prototype.hasOwnProperty.call(extra, k)) msg[k] = extra[k]; }
    }
    estado.mensajes.push(msg);
    pintar();
    guardar();
  }

  // --- envío ---------------------------------------------------------------

  // SI-113: mientras carga, el campo se deshabilita en vez de tragarse el siguiente
  // intento en silencio. SI-065: el botón de enviar se convierte en «Detener» — no hay
  // dos botones nuevos que la hoja de estilos no conozca, sólo cambia de función.
  function marcarCargando(cargando) {
    var i = document.getElementById("ia-input");
    var btn = document.getElementById("ia-enviar");
    if (i) { i.disabled = cargando; i.setAttribute("aria-busy", String(cargando)); }
    if (btn) {
      btn.innerHTML = cargando
        ? '<i class="fas fa-stop"></i>' : '<i class="fas fa-paper-plane"></i>';
      btn.setAttribute("aria-label", cargando ? "Detener" : "Enviar");
      btn.title = cargando ? "Detener" : "Enviar";
    }
  }

  // SI-065: corta la llamada que ya se está pagando. El servidor sigue sin enterarse de
  // la cancelación a media vuelta de herramientas (eso exige tocar `app/routes/ai.py`,
  // fuera de este archivo), pero al menos deja de esperar y de bloquear el input.
  function detener() {
    if (peticionActual) peticionActual.abort();
  }

  function enviar(texto) {
    texto = (texto || "").trim();
    if (!texto || estado.cargando) return;

    estado.mensajes.push({ rol: "user", contenido: texto });
    estado.cargando = true;
    marcarCargando(true);
    pintar();
    guardar();

    var controlador = (typeof AbortController !== "undefined") ? new AbortController() : null;
    peticionActual = controlador;

    fetch("/api/ia/chat", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      // La sesión viaja en la cookie HttpOnly: no hay token que este script pueda leer,
      // y por eso mismo no hay token que un XSS pueda robarle.
      credentials: "same-origin",
      signal: controlador ? controlador.signal : undefined,
      body: JSON.stringify({
        mensajes: estado.mensajes
          .filter(function (m) { return m.rol === "user" || m.rol === "assistant"; })
          .map(function (m) { return { rol: m.rol, contenido: m.contenido }; }),
        conversacion_id: estado.convId
      })
    })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        peticionActual = null;
        estado.cargando = false;
        marcarCargando(false);
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
          // SI-117: ya no navega solo. Se ofrece el destino en un botón y decide la persona.
          if (res.d.navegar_a) {
            estado.mensajes.push({
              rol: "sistema", contenido: "El asistente sugiere ir a otra página.",
              navegar_a: res.d.navegar_a
            });
          }
        }
        pintar();
        guardar();
      })
      .catch(function (err) {
        peticionActual = null;
        estado.cargando = false;
        marcarCargando(false);
        // SI-065: cancelado a propósito, no es un error de red — no hace falta el aviso
        // ni tiene sentido invitar a «intentar de nuevo».
        estado.mensajes.push({
          rol: "assistant",
          contenido: (err && err.name === "AbortError")
            ? "Mensaje cancelado." : "⚠️ Error de red. Intenta de nuevo."
        });
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
    // SI-115: el nombre se guarda crudo. `formatear()` ya escapa al pintar; escaparlo aquí
    // también lo dejaba doblemente escapado ("Acta 1 &amp; 2.pdf").
    sistema("Subiendo «" + archivo.name + "»…");

    fetch("/api/ia/adjuntar", { method: "POST", credentials: "same-origin", body: fd })
      .then(function (r) { return r.json().then(function (d) { return { ok: r.ok, d: d }; }); })
      .then(function (res) {
        if (!res.ok) {
          sistema("⚠️ " + (res.d.detail || "No se pudo subir el archivo."));
          return;
        }
        // SI-116: ya no se dispara un turno pago solo. Se deja el adjunto en el hilo con
        // una sugerencia pulsable; la persona decide si pregunta por él.
        sistema("📎 «" + res.d.nombre_archivo + "» subido.", {
          sugerencia: "Acabo de subir el archivo «" + res.d.nombre_archivo +
                      "». Revísalo con mis_adjuntos y dime a qué documento lo engancho."
        });
      })
      .catch(function () { sistema("⚠️ Error de red al subir el archivo."); });
  }

  // --- montaje -------------------------------------------------------------

  // SI-110: el panel no atrapaba el foco (se podía tabular fuera hacia la página de
  // debajo) ni se cerraba con Escape, y al cerrar el foco se quedaba donde estuviera en
  // vez de volver a la burbuja que lo abrió.
  function elementosFocables(cont) {
    return Array.prototype.slice.call(
      cont.querySelectorAll('button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])')
    ).filter(function (el) { return el.offsetParent !== null && !el.disabled; });
  }

  function alAtraparFoco(e) {
    if (e.key !== "Tab") return;
    var panel = document.getElementById("ia-panel");
    var focables = elementosFocables(panel);
    if (!focables.length) return;
    var primero = focables[0], ultimo = focables[focables.length - 1];
    if (e.shiftKey && document.activeElement === primero) {
      e.preventDefault(); ultimo.focus();
    } else if (!e.shiftKey && document.activeElement === ultimo) {
      e.preventDefault(); primero.focus();
    }
  }

  function alTeclaPanel(e) {
    if (e.key === "Escape") { alternar(); return; }
    alAtraparFoco(e);
  }

  function alternar() {
    estado.abierto = !estado.abierto;
    var panel = document.getElementById("ia-panel");
    panel.classList.toggle("ia-abierto", estado.abierto);
    if (estado.abierto) panel.removeAttribute("aria-hidden");
    else panel.setAttribute("aria-hidden", "true");
    var burbuja = document.getElementById("ia-burbuja");
    if (burbuja) {
      burbuja.setAttribute("aria-expanded", String(estado.abierto));
      burbuja.setAttribute("aria-label", estado.abierto ? "Cerrar asistente" : "Abrir asistente");
    }
    if (estado.abierto) {
      pintar();
      revalidarConvId();
      panel.addEventListener("keydown", alTeclaPanel);
      var i = document.getElementById("ia-input");
      if (i) i.focus();
    } else {
      panel.removeEventListener("keydown", alTeclaPanel);
      if (burbuja) burbuja.focus();
    }
  }

  function limpiar() {
    estado.mensajes = [];
    estado.convId = null;
    estado.propuestas = [];
    guardar();
    pintar();
  }

  // SI-119: no había forma de sacar la conversación del panel salvo copiarla a mano.
  function descargarConversacion() {
    var texto = estado.mensajes
      .filter(function (m) { return m.rol === "user" || m.rol === "assistant"; })
      .map(function (m) { return (m.rol === "user" ? "Tú: " : "Asistente: ") + m.contenido; })
      .join("\n\n");
    if (!texto) return;
    var blob = new Blob([texto], { type: "text/plain;charset=utf-8" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = "conversacion-asistente.txt";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }

  function montar() {
    if (document.getElementById("ia-burbuja")) return;

    var cont = document.createElement("div");
    cont.innerHTML =
      '<button id="ia-burbuja" title="Asistente del Archivo" aria-label="Abrir asistente" ' +
             'aria-expanded="false" aria-controls="ia-panel">' +
        '<i class="fas fa-robot"></i></button>' +
      '<div id="ia-panel" role="dialog" aria-modal="true" aria-hidden="true" ' +
             'aria-label="Asistente del Archivo">' +
        '<div class="ia-cab">' +
          '<span><i class="fas fa-robot"></i> Asistente del Archivo</span>' +
          "<div>" +
            '<button id="ia-hist-btn" title="Conversaciones anteriores" ' +
                    'aria-label="Conversaciones anteriores" style="display:none">' +
              '<i class="fas fa-clock-rotate-left"></i></button>' +
            '<button id="ia-limpiar" title="Nueva conversación" aria-label="Nueva conversación">' +
              '<i class="fas fa-plus"></i></button>' +
            '<button id="ia-descargar" title="Descargar conversación" ' +
                    'aria-label="Descargar conversación">' +
              '<i class="fas fa-download"></i></button>' +
            '<button id="ia-cerrar" title="Cerrar" aria-label="Cerrar asistente">' +
              '<i class="fas fa-times"></i></button>' +
          "</div></div>" +
        '<div id="ia-aviso"></div>' +
        '<div id="ia-historial">' +
          '<div class="ia-hist-cab">Conversaciones anteriores</div>' +
          '<div id="ia-hist-lista"></div>' +
        "</div>" +
        // SI-111: `role="log"` + `aria-live="polite"` para que un lector de pantalla
        // anuncie sólo lo que se añade, no toda la conversación reconstruida.
        '<div id="ia-mensajes" role="log" aria-live="polite" aria-relevant="additions"></div>' +
        '<form id="ia-form">' +
          '<button type="button" id="ia-clip" title="Adjuntar archivo" style="display:none">' +
            '<i class="fas fa-paperclip"></i></button>' +
          '<input id="ia-file" type="file" style="display:none">' +
          '<input id="ia-input" type="text" maxlength="2000" autocomplete="off" ' +
                 'placeholder="Pregunta sobre los documentos…">' +
          '<button type="submit" id="ia-enviar" aria-label="Enviar">' +
            '<i class="fas fa-paper-plane"></i></button>' +
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
      // SI-065: mientras carga, el mismo botón ya no envía — detiene.
      if (estado.cargando) { detener(); return; }
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
      if (sug) { enviar(sug.dataset.sugerencia || sug.textContent); return; }
      var ir = e.target.closest(".ia-ir");
      if (ir) { window.location.href = ir.dataset.href; return; }
      var cop = e.target.closest(".ia-copiar");
      if (cop) {
        var m = estado.mensajes[Number(cop.dataset.idx)];
        if (m && navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(m.contenido).catch(function () {});
        }
        return;
      }
      var ap = e.target.closest(".ia-aprobar");
      if (ap) { resolver(Number(ap.dataset.id), "aprobar"); return; }
      var re = e.target.closest(".ia-rechazar");
      if (re) { resolver(Number(re.dataset.id), "rechazar"); }
    });

    document.getElementById("ia-descargar").addEventListener("click", descargarConversacion);

    comprobarDisponibilidad(false);

    restaurar();
    pintar();

    vigilarModales();
  }

  // VI-004: la burbuja y el panel iban por encima del panel de personalización
  // y de cualquier modal (z-index:3000 contra --z-overlay:1050/--z-modal:1060),
  // tapando botones reales. El z-index ya baja por debajo en ai-widget.css;
  // esto además los atenúa y les quita el clic mientras haya un modal abierto,
  // con el mismo patrón que ya usan admin-ui.js/scanner-client.js.
  function actualizarVisibilidadModal() {
    var hayModal = !!document.querySelector(".modal.show");
    var burbuja = document.getElementById("ia-burbuja");
    var panel = document.getElementById("ia-panel");
    if (burbuja) burbuja.classList.toggle("ia-tapado", hayModal);
    if (panel) panel.classList.toggle("ia-tapado", hayModal);
  }

  function vigilarModales() {
    actualizarVisibilidadModal();
    if (typeof MutationObserver === "undefined") return;
    var obs = new MutationObserver(actualizarVisibilidadModal);
    obs.observe(document.body, {
      attributes: true, attributeFilter: ["class"], childList: true, subtree: true
    });
  }

  // SI-122: un corte de red de dos segundos ya no deja al usuario sin asistente el resto
  // de la sesión. Se reintenta una vez antes de darlo por caído.
  function comprobarDisponibilidad(esReintento) {
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
        if (!esReintento) {
          setTimeout(function () { comprobarDisponibilidad(true); }, 2000);
          return;
        }
        estado.disponible = false;
        pintar();
      });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", montar);
  } else {
    montar();
  }
})();
