// ==========================================================================
// CHOICES, TOM SELECT, FECHAS
// ==========================================================================
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
    console.error("Error al cargar choices dinÃ¡micos:", e);
  }
}

function _populateDataLists(rrhh) {
  if (!rrhh) return;
  const fill = (id, items) => {
    const dl = document.getElementById(id);
    if (!dl || !Array.isArray(items)) return;
    dl.innerHTML = items.map(v => `<option value="${String(v).replace(/"/g,'&quot;')}">`).join("");
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
  if (tsInstances["choice-archivo-tesauro"]) {
    tsInstances["choice-archivo-tesauro"].settings.load = (query, callback) => {
      if (!query.trim()) { callback([]); return; }
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

function initDateControls(module, data) {
  const input = document.getElementById(`fp-${module}-range`);
  if (input) {
    if (fpInstances[module]) fpInstances[module].destroy();
    fpInstances[module] = flatpickr(input, {
      mode: "range",
      dateFormat: "Y-m-d",
      minDate: data.min_date,
      maxDate: data.max_date,
      locale: {
        rangeSeparator: " â†’ ",
        firstDayOfWeek: 1,
        weekdays: {
          shorthand: ["Dom", "Lun", "Mar", "MiÃ©", "Jue", "Vie", "SÃ¡b"],
          longhand:  ["Domingo", "Lunes", "Martes", "MiÃ©rcoles", "Jueves", "Viernes", "SÃ¡bado"]
        },
        months: {
          shorthand: ["Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"],
          longhand:  ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"]
        }
      },
      onChange: (selectedDates) => {
        if (selectedDates.length === 2) {
          const fmt = d => d.toISOString().split("T")[0];
          state[module].dateStart = fmt(selectedDates[0]);
          state[module].dateEnd   = fmt(selectedDates[1]);
          state[module].page = 1;
          const lbl = document.getElementById(`fp-${module}-label`);
          if (lbl) lbl.innerText = `${formatISOToSpanish(state[module].dateStart)} â†’ ${formatISOToSpanish(state[module].dateEnd)}`;
          if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
        }
      }
    });
  }

  const sy = document.getElementById(`year-select-${module}`);
  if (sy) {
    // Usar lista exacta de aÃ±os con datos si estÃ¡ disponible (mÃ¡s precisa)
    const years = (data.years && data.years.length)
      ? data.years
      : (() => {
          const minY = parseInt(data.min_date.substring(0, 4));
          const maxY = parseInt(data.max_date.substring(0, 4));
          return Array.from({ length: maxY - minY + 1 }, (_, i) => maxY - i);
        })();
    sy.innerHTML = `<option value="">Seleccionar aÃ±oâ€¦</option>` +
      years.map(y => `<option value="${y}">${y}</option>`).join("");
  }

  state[module].dateStart = data.min_date;
  state[module].dateEnd   = data.max_date;
  _setChipActive(module, "all");
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `${formatISOToSpanish(data.min_date)} â†’ ${formatISOToSpanish(data.max_date)}`;
}

function _setChipActive(module, preset) {
  document.querySelectorAll(`.ds-date-chip[data-module="${module}"]`).forEach(btn => {
    btn.classList.toggle("active", btn.dataset.preset === preset);
  });
}

function applyDatePreset(module, preset) {
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
  const fmt   = d => d.toISOString().split("T")[0];
  let startDate, endDate = fmt(today);

  if (preset === "all") {
    startDate = lim?.min_date || fmt(new Date(today.getFullYear() - 10, 0, 1));
    endDate   = lim?.max_date || fmt(today);
  }

  state[module].dateStart = startDate;
  state[module].dateEnd   = endDate;
  state[module].page = 1;
  if (lbl) lbl.innerText = `${formatISOToSpanish(startDate)} â†’ ${formatISOToSpanish(endDate)}`;
  if (module === "archivo") triggerArchivoSearch(); else triggerRrhhSearch();
}

function handleYearSelect(module) {
  const sy = document.getElementById(`year-select-${module}`);
  if (!sy || !sy.value) return;
  const y = sy.value;
  state[module].dateStart = `${y}-01-01`;
  state[module].dateEnd   = `${y}-12-31`;
  state[module].page = 1;
  const lbl = document.getElementById(`fp-${module}-label`);
  if (lbl) lbl.innerText = `AÃ±o ${y}`;
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
  applyDatePreset(module, "all");
}

