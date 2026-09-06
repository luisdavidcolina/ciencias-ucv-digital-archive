// ==========================================================================
// CHOICES, TOM SELECT, FECHAS
// ==========================================================================

// BA-177: cargar la localización oficial de flatpickr en vez de reimplementarla
// a mano. Se inyecta la misma versión que ya usa cada página (ver <script>
// de flatpickr en archive.html/hr.html) y se cachea la promesa para no
// duplicar la carga entre módulos.
let _fpEsLocalePromise = null;
function _loadFlatpickrSpanishLocale() {
  if (_fpEsLocalePromise) return _fpEsLocalePromise;
  _fpEsLocalePromise = new Promise(resolve => {
    if (typeof flatpickr === "undefined") { resolve(null); return; }
    if (flatpickr.l10ns && flatpickr.l10ns.es) { resolve(flatpickr.l10ns.es); return; }
    const s = document.createElement("script");
    s.src = "https://cdn.jsdelivr.net/npm/flatpickr@4.6.13/dist/l10n/es.js";
    s.onload = () => resolve((flatpickr.l10ns && flatpickr.l10ns.es) || null);
    s.onerror = () => resolve(null);
    document.head.appendChild(s);
  });
  return _fpEsLocalePromise;
}

async function loadDynamicChoices() {
  try {
    const res = await fetch(`${API_BASE}/api/choices`);
    if (!res.ok) throw new Error();
    const data = await res.json();
    state.choices = data;
    state.archivo.dateStart = data.archivo.min_date;
    state.archivo.dateEnd   = data.archivo.max_date;
    state.rrhh.dateStart    = data.rrhh.min_date;
    state.rrhh.dateEnd      = data.rrhh.max_date;
    initTomSelects();
    initDateControls("archivo", data.archivo);
    initDateControls("rrhh",    data.rrhh);
    _populateDataLists(data.rrhh);
  } catch (e) {
    console.error("Error al cargar choices dinámicos:", e);
  }
}

function _populateDataLists(rrhh) {
  if (!rrhh) return;
  const fill = (id, items) => {
    const dl = document.getElementById(id);
    if (!dl || !Array.isArray(items)) return;
    dl.innerHTML = items.map(v => `<option value="${escHtml(String(v))}">`).join("");
  };
  fill("dl-cargos",       rrhh.cargos);
  fill("dl-departamentos", rrhh.departamentos);
}

function initTomSelects() {
  if (!state.choices || typeof TomSelect === "undefined") return;

  function makeSel(id, items, onChange) {
    const el = document.getElementById(id);
    if (!el) return;
    if (tsInstances[id]) { tsInstances[id].destroy(); delete tsInstances[id]; }
    tsInstances[id] = new TomSelect(el, {
      plugins: ["remove_button"],
      create: false,
      maxOptions: null,
      options: items.map(v => ({ value: v, text: v })),
      items: [],
      placeholder: el.getAttribute("placeholder") || "Seleccionar...",
      onChange
    });
  }

  makeSel("choice-archivo-doc-type", state.choices.archivo.doc_types, val => {
    state.archivo.selectedTypes  = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  makeSel("choice-archivo-tesauro", [], val => {
    state.archivo.selectedTesauro = Array.isArray(val) ? val : (val ? [val] : []);
    state.archivo.page = 1; triggerArchivoSearch();
  });
  // Configurar carga remota para el Tom Select de palabras clave
  // BA-027: longitud mínima antes de golpear al backend, y loadThrottle para
  // no lanzar un escaneo completo por cada pulsación de tecla.
  if (tsInstances["choice-archivo-tesauro"]) {
    tsInstances["choice-archivo-tesauro"].settings.loadThrottle = 300;
    tsInstances["choice-archivo-tesauro"].settings.load = (query, callback) => {
      if (query.trim().length < 2) { callback([]); return; }
      fetch(`${API_BASE}/api/archivo/documentos/buscar?q=${encodeURIComponent(query)}`)
        .then(r => r.json())
        .then(data => callback(data.map(d => ({ value: d.nombre_corto, text: d.nombre_corto }))))
        .catch(() => callback([]));
    };
  }
  makeSel("choice-rrhh-doc-type", state.choices.rrhh.doc_types, val => {
    state.rrhh.selectedTypes  = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
  makeSel("choice-rrhh-estado", state.choices.rrhh.estados, val => {
    state.rrhh.selectedEstados = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
  makeSel("choice-rrhh-people", state.choices.rrhh.people, val => {
    state.rrhh.selectedPeople = Array.isArray(val) ? val : (val ? [val] : []);
    state.rrhh.page = 1; triggerRrhhSearch();
  });
}

// BA-010: nunca formatear una fecha local con toISOString() — convierte a UTC
// y en Venezuela (UTC-4) desplaza el día seleccionado. Usar los getters locales.
function _fmtLocalISODate(d) {
  const y = d.getFullYear();
  const m = String(d.getMonth() + 1).padStart(2, "0");
  const day = String(d.getDate()).padStart(2, "0");
  return `${y}-${m}-${day}`;
}

async function initDateControls(module, data) {
  const input = document.getElementById(`fp-${module}-range`);
  if (input) {
    if (fpInstances[module]) fpInstances[module].destroy();
    const esLocale = await _loadFlatpickrSpanishLocale();
    fpInstances[module] = flatpickr(input, {
      mode: "range",
      dateFormat: "Y-m-d",
      minDate: data.min_date,
      maxDate: data.max_date,
      locale: esLocale || {
        rangeSeparator: " → ",
        firstDayOfWeek: 1,
        weekdays: {
          shorthand: ["Dom", "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb"],
          longhand:  ["Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"]
        },
        months: {
          shorthand: ["Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"],
          longhand:  ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"]
        }
      },
      onChange: (selectedDates) => {
        if (selectedDates.length === 2) {
          state[module].dateStart = _fmtLocalISODate(selectedDates[0]);
          state[module].dateEnd   = _fmtLocalISODate(selectedDates[1]);
          state[module].page = 1;
          const lbl = document.getElementById(`fp-${module}-label`);
          if (lbl) lbl.innerText = `${formatISOToSpanish(state[module].dateStart)} → ${formatISOToSpanish(state[module].dateEnd)}`;
          if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
        }
      }
    });
  }

  const sy = document.getElementById(`year-select-${module}`);
  if (sy) {
    // Usar lista exacta de años con datos si está disponible (más precisa)
    const years = (data.years && data.years.length)
      ? data.years
      : (() => {
          const minY = parseInt(data.min_date.substring(0, 4));
          const maxY = parseInt(data.max_date.substring(0, 4));
          return Array.from({ length: maxY - minY + 1 }, (_, i) => maxY - i);
        })();
    sy.innerHTML = `<option value="">Seleccionar año…</option>` +
      years.map(y => `<option value="${y}">${y}</option>`).join("");
  }

  state[module].dateStart = data.min_date;
  state[module].dateEnd   = data.max_date;
  _setChipActive(module, "all");
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `${formatISOToSpanish(data.min_date)} → ${formatISOToSpanish(data.max_date)}`;
}

function _setChipActive(module, preset) {
  // BA-094: los chips son un role="radio" dentro de un radiogroup (archive.html);
  // el estado activo tiene que reflejarse en aria-checked, no sólo en el color.
  document.querySelectorAll(`.ds-date-chip[data-module="${module}"]`).forEach(btn => {
    const isActive = btn.dataset.preset === preset;
    btn.classList.toggle("active", isActive);
    btn.setAttribute("aria-checked", isActive ? "true" : "false");
  });
}

// BA-009: `search=false` permite reposicionar el filtro sin disparar una
// búsqueda — lo usa resetDateFilters(), cuyo propio invocador ya busca una
// vez por su cuenta (ver btn_clear_archivo/btn_clear_rrhh en app.js).
function applyDatePreset(module, preset, search = true) {
  _setChipActive(module, preset);
  const yearPanel  = document.getElementById(`year-panel-${module}`);
  const rangePanel = document.getElementById(`range-panel-${module}`);
  const lbl = document.getElementById(`fp-${module}-label`);
  const lim = state.choices?.[module];

  if (preset === "year") {
    if (yearPanel)  yearPanel.style.display  = "";
    if (rangePanel) rangePanel.style.display = "none";
    if (lbl) lbl.innerText = "";
    return;
  }
  if (preset === "custom") {
    if (yearPanel)  yearPanel.style.display  = "none";
    if (rangePanel) rangePanel.style.display = "";
    if (lbl) lbl.innerText = "";
    return;
  }

  if (yearPanel)  yearPanel.style.display  = "none";
  if (rangePanel) rangePanel.style.display = "none";

  const today = new Date();
  const fmt   = _fmtLocalISODate;
  let startDate, endDate = fmt(today);

  if (preset === "all") {
    startDate = lim?.min_date || fmt(new Date(today.getFullYear() - 10, 0, 1));
    endDate   = lim?.max_date || fmt(today);
  }

  state[module].dateStart = startDate;
  state[module].dateEnd   = endDate;
  state[module].page = 1;
  if (lbl) lbl.innerText = `${formatISOToSpanish(startDate)} → ${formatISOToSpanish(endDate)}`;
  if (search) {
    if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
  }
}

function handleYearSelect(module) {
  const sy = document.getElementById(`year-select-${module}`);
  if (!sy || !sy.value) return;
  const y = sy.value;
  state[module].dateStart = `${y}-01-01`;
  state[module].dateEnd   = `${y}-12-31`;
  state[module].page = 1;
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `Año ${y}`;
  if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
}


function resetDateFilters(module) {
  state[module].search       = "";
  state[module].selectedTypes = [];
  state[module].page = 1;
  if (module === "archivo") {
    state.archivo.selectedTesauro = [];
    const s = document.getElementById("search_archivo");
    if (s) s.value = "";
    if (tsInstances["choice-archivo-doc-type"]) tsInstances["choice-archivo-doc-type"].clear(true);
    if (tsInstances["choice-archivo-tesauro"])  tsInstances["choice-archivo-tesauro"].clear(true);
  } else {
    state.rrhh.selectedEstados = [];
    state.rrhh.selectedPeople  = [];
    const s = document.getElementById("search_rrhh");
    if (s) s.value = "";
    if (tsInstances["choice-rrhh-doc-type"]) tsInstances["choice-rrhh-doc-type"].clear(true);
    if (tsInstances["choice-rrhh-estado"])   tsInstances["choice-rrhh-estado"].clear(true);
    if (tsInstances["choice-rrhh-people"])   tsInstances["choice-rrhh-people"].clear(true);
  }
  // BA-009: no buscar aquí — el handler del botón "Limpiar" (app.js) ya
  // dispara su propia búsqueda justo después de llamar a esta función.
  applyDatePreset(module, "all", false);
}

