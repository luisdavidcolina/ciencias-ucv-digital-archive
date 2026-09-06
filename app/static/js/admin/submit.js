// --- NUEVO INGRESO ---
// Pendientes de la auditoría (OA-084 a OA-102, OR-081 a OR-099) resueltos dentro de
// este archivo: validación en el borde, mensajes por campo, borrador local,
// subida con progreso real, cédula/RIF venezolanos y coherencia de fechas.

// Campos obligatorios declarados una sola vez (OA-086 / OR-087): el marcado,
// el validador y el resumen de "campos obligatorios" leen de aquí.
const SUBMIT_REQUIRED_FIELDS = {
  archivo: [
    { id: "reg-title",         label: "Título del Documento" },
    { id: "reg-author",        label: "Autor / Ente Emisor" },
    { id: "reg-doc-type",      label: "Tipo de Documento" },
    { id: "reg-secundario",    label: "Clasificación" },
    { id: "reg-fecha",         label: "Fecha de Emisión" },
    { id: "reg-descriptores",  label: "Palabras Clave" },
    { id: "reg-location",      label: "Ubicación Física" }
  ],
  rrhh: [
    { id: "reg-nombres",   label: "Nombres" },
    { id: "reg-apellidos", label: "Apellidos" },
    { id: "reg-cedula",    label: "Cédula de Identidad" },
    { id: "reg-doc-type",  label: "Tipo de Documento" },
    { id: "reg-depto",     label: "Departamento" },
    { id: "reg-estado",    label: "Estado" },
    { id: "reg-fecha",     label: "Fecha de Ingreso" },
    { id: "reg-location",  label: "Ubicación física" }
  ]
};

// Topes de longitud (OA-085), calcados de `models.py` para fallar en el cliente
// antes de que lo haga Pydantic con un 422 genérico.
const SUBMIT_MAXLENGTH = {
  "reg-title": 500, "reg-author": 255, "reg-resumen": 4000,
  "reg-descriptores": 2000, "reg-folio": 100, "reg-personas-archivo": 1000,
  "reg-nombres": 200, "reg-apellidos": 200, "reg-cedula": 20, "reg-rif": 20,
  "reg-cargo": 200, "reg-depto": 200, "reg-personas": 1000, "reg-foto": 2048
};

// Tipos y tamaño máximo aceptados para el archivo digitalizado (OR-097 / OA-093),
// coherente con la lista canónica ya usada en el modal de edición.
const SUBMIT_ACCEPT_TYPES = [
  "application/pdf", "image/jpeg", "image/png", "image/tiff", "image/webp"
];
const SUBMIT_MAX_FILE_MB = 25;

function renderDynamicSubmitFields() {
  const suf       = adminSuffixFromTab();
  const container = document.getElementById(`dynamic-submit-fields-${suf}`);
  if (!container) return;

  if (isArchivoModule()) {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-title-${suf}">Título del Documento <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-title-${suf}" class="form-control" placeholder="Ej: Plan Regulador de Áreas Verdes" required aria-required="true" maxlength="500">
          <div class="invalid-feedback" id="reg-title-${suf}-error"></div>
          <small class="form-text text-muted d-none" id="reg-title-${suf}-count"></small>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-author-${suf}">Autor / Ente Emisor <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-author-${suf}" class="form-control" placeholder="Ej: Arq. Villanueva" required aria-required="true" maxlength="255">
          <div class="invalid-feedback" id="reg-author-${suf}-error"></div>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-doc-type-${suf}">Tipo de Documento <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <select id="reg-doc-type-${suf}" class="form-control" required aria-required="true">
            ${(state.choices?.archivo?.doc_types || ["Proyecto de Investigación","Informe","Plano Arquitectónico","Acta de Sesión","Resolución","Reglamento"]).map(t => `<option value="${escHtml(t)}">${escHtml(t)}</option>`).join("")}
          </select>
          <div class="invalid-feedback" id="reg-doc-type-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-secundario-${suf}">Clasificación <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <select id="reg-secundario-${suf}" class="form-control" required aria-required="true">
            <option value="Parte I">Parte I</option>
            <option value="Parte II">Parte II</option>
            <option value="Parte III">Parte III</option>
            <option value="Parte IV">Parte IV</option>
          </select>
          <div class="invalid-feedback" id="reg-secundario-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-fecha-${suf}">Fecha de Emisión <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required aria-required="true">
          <div class="invalid-feedback" id="reg-fecha-${suf}-error"></div>
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted" for="reg-descriptores-${suf}">Palabras Clave (separadas por coma) <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
        <input type="text" id="reg-descriptores-${suf}" class="form-control" placeholder="Ej: Planificación académica, Gestión institucional" required aria-required="true" maxlength="2000">
        <div class="invalid-feedback" id="reg-descriptores-${suf}-error"></div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted" for="reg-resumen-${suf}">Resumen Descriptivo (Abstract)</label>
        <textarea id="reg-resumen-${suf}" class="form-control" rows="3" placeholder="Breve síntesis o abstract del documento..." maxlength="4000"></textarea>
        <small class="form-text text-muted d-none" id="reg-resumen-${suf}-count"></small>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted" for="reg-location-${suf}">Ubicación Física (Estante, Gaveta o Caja) <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
        <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Mapoteca - Gaveta 1 o Digitalizado Exclusivo" required aria-required="true" maxlength="500">
        <div class="invalid-feedback" id="reg-location-${suf}-error"></div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-folio-${suf}">N° de Folio / Signatura</label>
          <input type="text" id="reg-folio-${suf}" class="form-control" placeholder="Ej: F-023, Carp-A-12" maxlength="100">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-soporte-${suf}">Soporte</label>
          <select id="reg-soporte-${suf}" class="form-control">
            ${(state.choices?.archivo?.soportes || ["Físico","Digital","Digitalizado"]).map(s => `<option value="${escHtml(s)}">${escHtml(s)}</option>`).join("")}
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-paginas-${suf}">N° de Páginas</label>
          <input type="number" id="reg-paginas-${suf}" class="form-control" min="1" placeholder="Ej: 12">
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-vencimiento-${suf}">Fecha de Vencimiento <span class="badge badge-warning text-dark badge-sm">Retención</span></label>
          <input type="date" id="reg-vencimiento-${suf}" class="form-control">
          <small class="text-muted">Opcional — sobreescribe el plazo del tipo de documento</small>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-personas-archivo-${suf}">Personas / Dependencias Relacionadas</label>
          <input type="text" id="reg-personas-archivo-${suf}" class="form-control" placeholder="Ej: Decano Flores; Comisión Curricular" maxlength="1000">
        </div>
      </div>
      <div class="form-group mt-2" id="reg-file-zone-${suf}">
        ${_submitFileZoneMarkup(suf)}
      </div>
      <small class="text-muted d-block mt-2" id="reg-draft-hint-${suf}"></small>
    `;
  } else {
    container.innerHTML = `
      <div class="row">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-nombres-${suf}">Nombres <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-nombres-${suf}" class="form-control" placeholder="Ej: Susana María" required aria-required="true" maxlength="200">
          <div class="invalid-feedback" id="reg-nombres-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-apellidos-${suf}">Apellidos <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-apellidos-${suf}" class="form-control" placeholder="Ej: Pérez González" required aria-required="true" maxlength="200">
          <div class="invalid-feedback" id="reg-apellidos-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-cedula-${suf}">Cédula de Identidad <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <div class="input-group">
            <input type="text" id="reg-cedula-${suf}" class="form-control" placeholder="Ej: V-12345678" required aria-required="true" maxlength="20">
            <div class="input-group-append">
              <button class="btn btn-outline-info btn-sm" type="button" title="Buscar empleado por cédula"
                      onclick="_lookupByCedula('${suf}')">
                <i class="fas fa-user-check"></i>
              </button>
            </div>
          </div>
          <div class="invalid-feedback" id="reg-cedula-${suf}-error"></div>
          <small id="reg-cedula-hint-${suf}" class="text-muted"></small>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-rif-${suf}">C.I.F. (RIF)</label>
          <input type="text" id="reg-rif-${suf}" class="form-control" placeholder="Ej: J-12345678-0" maxlength="20">
          <div class="invalid-feedback" id="reg-rif-${suf}-error"></div>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-cargo-${suf}">Cargo Asignado</label>
          <input type="text" id="reg-cargo-${suf}" class="form-control" placeholder="Ej: Analista Contable" list="dl-cargos" autocomplete="off" maxlength="200">
        </div>
      </div>
      <div class="form-group mt-2">
        <label class="font-weight-bold text-muted" for="reg-personas-${suf}">Personas / Dependencias Relacionadas</label>
        <input type="text" id="reg-personas-${suf}" class="form-control" placeholder="Ej: Susana Pérez; Dirección RRHH" maxlength="1000">
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-doc-type-${suf}">Tipo de Documento <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <select id="reg-doc-type-${suf}" class="form-control" required aria-required="true">
            ${(function() {
              const tiposPP = state.choices?.rrhh?.tipos_por_parte || {};
              if (Object.keys(tiposPP).length > 0) {
                return Object.entries(tiposPP).map(([parte, tipos]) =>
                  `<optgroup label="${escHtml(parte)}">${tipos.map(t => `<option value="${escHtml(t)}">${escHtml(t)}</option>`).join("")}</optgroup>`
                ).join("");
              }
              return (state.choices?.rrhh?.doc_types || ["Hoja de Vida","Contrato"]).map(t => `<option value="${escHtml(t)}">${escHtml(t)}</option>`).join("");
            })()}
          </select>
          <div class="invalid-feedback" id="reg-doc-type-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-depto-${suf}">Departamento <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-depto-${suf}" class="form-control" placeholder="Ej: Biología" required aria-required="true" list="dl-departamentos" autocomplete="off" maxlength="200">
          <div class="invalid-feedback" id="reg-depto-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-estado-${suf}">Estado <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <select id="reg-estado-${suf}" class="form-control" required aria-required="true">
            ${(state.choices?.rrhh?.estados_catalog || ["Activo","Retirado","Jubilado","Pensionado"])
              .map(e => `<option value="${escHtml(e)}"${e==="Activo"?" selected":""}>${escHtml(e)}</option>`).join("")}
          </select>
          <div class="invalid-feedback" id="reg-estado-${suf}-error"></div>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-fecha-${suf}">Fecha de Ingreso <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="date" id="reg-fecha-${suf}" class="form-control" required aria-required="true">
          <div class="invalid-feedback" id="reg-fecha-${suf}-error"></div>
        </div>
        <div class="col-md-6 form-group">
          <label class="font-weight-bold text-muted" for="reg-location-${suf}">Ubicación física <abbr title="obligatorio" aria-hidden="true">*</abbr></label>
          <input type="text" id="reg-location-${suf}" class="form-control" placeholder="Ej: Archivo Central - Caja J-02" required aria-required="true" maxlength="500">
          <div class="invalid-feedback" id="reg-location-${suf}-error"></div>
        </div>
      </div>
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-jubilacion-${suf}">Fecha de Jubilación (Opcional)</label>
          <input type="date" id="reg-jubilacion-${suf}" class="form-control">
          <div class="invalid-feedback" id="reg-jubilacion-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-pension-${suf}">Fecha de Pensión (Opcional)</label>
          <input type="date" id="reg-pension-${suf}" class="form-control">
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-foto-${suf}">URL Foto Avatar (Opcional)</label>
          <input type="text" id="reg-foto-${suf}" class="form-control" placeholder="https://..." maxlength="2048">
        </div>
      </div>
      <!-- Datos personales LOTTT -->
      <div class="row mt-2">
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-nacimiento-${suf}">Fecha de Nacimiento <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <input type="date" id="reg-nacimiento-${suf}" class="form-control">
          <div class="invalid-feedback" id="reg-nacimiento-${suf}-error"></div>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-sexo-${suf}">Sexo <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <select id="reg-sexo-${suf}" class="form-control">
            <option value="">No especificado</option>
            <option value="M">Masculino</option>
            <option value="F">Femenino</option>
            <option value="O">Otro</option>
          </select>
        </div>
        <div class="col-md-4 form-group">
          <label class="font-weight-bold text-muted" for="reg-nivel-${suf}">Nivel Educativo <span class="badge badge-secondary badge-sm">LOTTT</span></label>
          <select id="reg-nivel-${suf}" class="form-control">
            <option value="">No especificado</option>
            ${(state.choices?.rrhh?.niveles_educativos || ["Bachiller","TSU","Universitario","Especialización","Maestría","Doctorado","Postdoctorado"]).map(n => `<option value="${escHtml(n)}">${escHtml(n)}</option>`).join("")}
          </select>
        </div>
      </div>
      <div class="form-group mt-2" id="reg-file-zone-${suf}">
        ${_submitFileZoneMarkup(suf)}
      </div>
      <small class="text-muted d-block mt-2" id="reg-draft-hint-${suf}"></small>
    `;
  }

  _wireSubmitFieldEvents(suf);
  _restoreSubmitDraft(suf);
}

// ─── Zona de archivo digitalizado con selector nativo (OR-097 / OA-093) ───────
// El arrastrar-y-soltar vive en admin-monitor.js (carril B5); este formulario
// sólo declara el input nativo con `accept` y valida tipo/tamaño al elegir.
function _submitFileZoneMarkup(suf) {
  return `
    <label class="font-weight-bold text-muted" for="file_upload-${suf}">Archivo Digitalizado (Opcional)</label>
    <input type="file" id="file_upload-${suf}" class="form-control-file"
           accept="${SUBMIT_ACCEPT_TYPES.join(",")}">
    <small class="text-muted d-block">PDF, JPG, PNG, TIFF o WEBP — máximo ${SUBMIT_MAX_FILE_MB} MB.</small>
    <div class="invalid-feedback" id="file_upload-${suf}-error"></div>
    <div id="reg-file-info-${suf}" class="small mt-1"></div>
    <div id="reg-upload-progress-${suf}" class="mt-1 d-none">
      <div class="progress" style="height:8px;">
        <div class="progress-bar bg-primary" role="progressbar" style="width:0%;" id="reg-upload-bar-${suf}"></div>
      </div>
      <div class="d-flex justify-content-between align-items-center mt-1">
        <small class="text-muted" id="reg-upload-pct-${suf}">0%</small>
        <button type="button" class="btn btn-link btn-sm p-0 text-danger" id="reg-upload-cancel-${suf}">Cancelar subida</button>
      </div>
    </div>
  `;
}

// ─── Validación por campo (OA-084, OA-085, OA-088, OR-087, OR-088) ────────────

function _submitFieldEls(suf, fieldId) {
  return {
    input: document.getElementById(`${fieldId}-${suf}`),
    error: document.getElementById(`${fieldId}-${suf}-error`)
  };
}

function _setSubmitFieldError(suf, fieldId, message) {
  const { input, error } = _submitFieldEls(suf, fieldId);
  if (!input) return;
  if (message) {
    input.classList.add("is-invalid");
    input.setAttribute("aria-invalid", "true");
    input.setAttribute("aria-describedby", `${fieldId}-${suf}-error`);
    if (error) error.textContent = message;
  } else {
    input.classList.remove("is-invalid");
    input.removeAttribute("aria-invalid");
    if (error) error.textContent = "";
  }
}

// Valida un campo individual y devuelve el mensaje de error (o "" si es válido).
// Se usa tanto en `blur` como en el envío, para que sea la misma regla en los
// dos sitios (OA-085).
function _validateSubmitField(suf, fieldId, isArchivo) {
  const { input } = _submitFieldEls(suf, fieldId);
  if (!input) return "";
  const value = (input.value || "").trim();
  const required = SUBMIT_REQUIRED_FIELDS[isArchivo ? "archivo" : "rrhh"].some(f => f.id === fieldId);
  const label = (SUBMIT_REQUIRED_FIELDS[isArchivo ? "archivo" : "rrhh"].find(f => f.id === fieldId) || {}).label || fieldId;

  if (required && !value) return `${label} es obligatorio.`;
  if (!value) return "";

  const max = SUBMIT_MAXLENGTH[fieldId];
  if (max && value.length > max) return `Máximo ${max} caracteres (van ${value.length}).`;

  if (fieldId === "reg-fecha" && !isArchivo) {
    const err = _validateFechaIngreso(value);
    if (err) return err;
  }
  if (fieldId === "reg-fecha" && isArchivo) {
    const hoy = new Date().toISOString().substring(0, 10);
    if (value > hoy) return ""; // aviso, no bloqueo (se muestra aparte)
  }
  if (fieldId === "reg-cedula" && !isArchivo) {
    if (!validarCedulaVenezolana(value)) return "Formato inválido. Use V-12345678 o E-12345678.";
  }
  if (fieldId === "reg-rif" && !isArchivo) {
    if (!validarRifVenezolano(value)) return "Formato inválido. Use J-12345678-0, con dígito verificador correcto.";
  }
  if (fieldId === "reg-nacimiento" && !isArchivo) {
    const err = _validateCoherenciaFechas(suf);
    if (err) return err;
  }
  return "";
}

function _validateFechaIngreso(value) {
  const hoy = new Date().toISOString().substring(0, 10);
  if (value > hoy) return "La fecha de ingreso no puede ser futura.";
  return "";
}

// OR-085: nacimiento < ingreso < jubilación, edad al ingreso entre 16 y 75.
// Se muestra bajo "Fecha de Nacimiento" para no repetir el aviso en tres campos.
function _validateCoherenciaFechas(suf) {
  const nacStr = document.getElementById(`reg-nacimiento-${suf}`)?.value;
  const ingStr = document.getElementById(`reg-fecha-${suf}`)?.value;
  const jubStr = document.getElementById(`reg-jubilacion-${suf}`)?.value;
  if (!nacStr) return "";

  const nac = new Date(nacStr);
  if (ingStr) {
    const ing = new Date(ingStr);
    if (nac >= ing) return "La fecha de nacimiento debe ser anterior al ingreso.";
    const edadIngreso = (ing - nac) / (1000 * 60 * 60 * 24 * 365.25);
    if (edadIngreso < 16) return "Edad al ingreso menor de 16 años. Revise las fechas.";
    if (edadIngreso > 75) return "Edad al ingreso mayor de 75 años. Revise las fechas.";
  }
  if (jubStr && ingStr && new Date(jubStr) <= new Date(ingStr)) {
    return "La fecha de jubilación debe ser posterior al ingreso.";
  }
  return "";
}

// OR-084: cédula venezolana V|E-####### a ########.
function validarCedulaVenezolana(value) {
  return /^[VvEe]-?\d{6,8}$/.test((value || "").trim());
}

// OR-084: RIF J|G|V|E-########-# con dígito verificador (algoritmo SENIAT).
function validarRifVenezolano(value) {
  const v = (value || "").trim().toUpperCase().replace(/\s/g, "");
  const m = v.match(/^([JGVE])-?(\d{8,9})-?(\d)$/);
  if (!m) return false;
  const [, letra, numero, dv] = m;
  // Algoritmo SENIAT: peso 4 para el código de tipo (J/G/V/E) y pesos
  // decrecientes para los 8 primeros dígitos numéricos del RIF.
  const tipoPeso = { J: 2, G: 3, V: 4, E: 5 }[letra];
  const ocho = numero.padStart(8, "0").slice(-8).split("").map(Number);
  const pesosResto = [3, 2, 7, 6, 5, 4, 3, 2];
  let suma = tipoPeso * 4;
  for (let i = 0; i < 8; i++) suma += ocho[i] * pesosResto[i];
  let resto = suma % 11;
  let calc = 11 - resto;
  if (calc >= 10) calc = 0;
  return calc === Number(dv);
}

function _wireSubmitFieldEvents(suf) {
  const isArchivo = isArchivoModule();
  const fields = SUBMIT_REQUIRED_FIELDS[isArchivo ? "archivo" : "rrhh"].map(f => f.id)
    .concat(["reg-rif", "reg-nacimiento", "reg-jubilacion", "reg-resumen"]);

  fields.forEach(fieldId => {
    const { input } = _submitFieldEls(suf, fieldId);
    if (!input) return;
    input.addEventListener("blur", () => {
      const err = _validateSubmitField(suf, fieldId, isArchivo);
      _setSubmitFieldError(suf, fieldId, err);
    });
    input.addEventListener("input", () => {
      _markSubmitDirty(suf);
      if (input.classList.contains("is-invalid")) {
        _setSubmitFieldError(suf, fieldId, _validateSubmitField(suf, fieldId, isArchivo));
      }
      _updateSubmitCharCount(suf, fieldId);
    });
  });

  ["reg-title", "reg-resumen"].forEach(fieldId => _updateSubmitCharCount(suf, fieldId));

  // OA-098: fecha de emisión vacía y obligatoria; aviso si queda en el futuro.
  const fechaInput = document.getElementById(`reg-fecha-${suf}`);
  if (fechaInput) {
    fechaInput.value = "";
    fechaInput.addEventListener("change", () => {
      const hoy = new Date().toISOString().substring(0, 10);
      if (fechaInput.value && fechaInput.value > hoy) {
        showToast("La fecha ingresada es futura. Verifíquela.", "warning");
      }
    });
  }

  // OA-097 / OR-086: aviso de posible duplicado al salir del campo clave.
  if (isArchivo) {
    const titleInput = document.getElementById(`reg-title-${suf}`);
    if (titleInput) titleInput.addEventListener("blur", () => _checkPossibleDuplicateArchivo(suf));
  } else {
    const cedulaInput = document.getElementById(`reg-cedula-${suf}`);
    if (cedulaInput) {
      cedulaInput.addEventListener("blur", () => {
        const val = cedulaInput.value.trim();
        if (val) _lookupByCedula(suf);
      });
    }
  }

  // Archivo seleccionado: validar tipo/tamaño y mostrar nombre + tamaño (OR-097, OA-093).
  const fileInput = document.getElementById(`file_upload-${suf}`);
  if (fileInput) {
    fileInput.addEventListener("change", () => _handleSubmitFileSelected(suf));
  }

  // Cambios en cualquier control marcan el borrador como sucio (OA-089 / OR-089).
  const form = document.getElementById(`admin-submit-form-${suf}`);
  if (form) {
    form.addEventListener("input", () => _markSubmitDirty(suf));
    form.addEventListener("change", () => _markSubmitDirty(suf));
  }
}

function _updateSubmitCharCount(suf, fieldId) {
  const max = SUBMIT_MAXLENGTH[fieldId];
  if (!max) return;
  const { input } = _submitFieldEls(suf, fieldId);
  const countEl = document.getElementById(`${fieldId}-${suf}-count`);
  if (!input || !countEl) return;
  const len = (input.value || "").length;
  if (len > max * 0.7) {
    countEl.textContent = `${len} / ${max} caracteres`;
    countEl.classList.remove("d-none");
    countEl.classList.toggle("text-danger", len > max);
  } else {
    countEl.classList.add("d-none");
  }
}

// Consulta liviana contra /list_all (ya existente) por título; sin bloquear el
// alta. La comprobación exacta con fecha queda para cuando exista un endpoint
// dedicado (anotado en _BUZON.md, requiere tocar app/routes/admin/docs.py).
async function _checkPossibleDuplicateArchivo(suf) {
  const titleInput = document.getElementById(`reg-title-${suf}`);
  const title = titleInput?.value.trim();
  const hintId = `reg-title-${suf}-error`;
  if (!title || title.length < 4) return;
  try {
    const data = await apiFetchJSON(`${API_BASE}/api/admin/list_all?modulo=Archivo&search=${encodeURIComponent(title)}&per_page=5`);
    const match = (data.records || []).find(r => (r.titulo || "").trim().toLowerCase() === title.toLowerCase());
    if (match) {
      showToast(`Ya existe un documento con título similar: «${match.titulo}».`, "warning");
      const errEl = document.getElementById(hintId);
      if (errEl && !titleInput.classList.contains("is-invalid")) {
        errEl.textContent = `Posible duplicado: «${match.titulo}» ya está registrado.`;
        errEl.classList.add("text-warning");
      }
    }
  } catch {
    // La comprobación de duplicados es de cortesía; un fallo de red no bloquea el alta.
  }
}

function _handleSubmitFileSelected(suf) {
  const fileInput = document.getElementById(`file_upload-${suf}`);
  const infoEl     = document.getElementById(`reg-file-info-${suf}`);
  const file = fileInput?.files?.[0];
  _setSubmitFieldError(suf, "file_upload", "");
  if (!file) { if (infoEl) infoEl.innerHTML = ""; return; }

  const sizeMb = file.size / (1024 * 1024);
  const typeOk = SUBMIT_ACCEPT_TYPES.includes(file.type) || /\.(pdf|jpe?g|png|tiff?|webp)$/i.test(file.name);
  const errors = [];
  if (!typeOk) errors.push(`Tipo de archivo no admitido (${file.type || "desconocido"}).`);
  if (sizeMb > SUBMIT_MAX_FILE_MB) errors.push(`El archivo pesa ${sizeMb.toFixed(1)} MB; el máximo es ${SUBMIT_MAX_FILE_MB} MB.`);

  if (errors.length) {
    _setSubmitFieldError(suf, "file_upload", errors.join(" "));
    if (infoEl) infoEl.innerHTML = "";
    fileInput.value = "";
    return;
  }

  if (infoEl) {
    infoEl.innerHTML = `
      <span class="text-muted"><i class="fas fa-paperclip mr-1"></i>${escHtml(file.name)} — ${sizeMb.toFixed(2)} MB</span>
      <button type="button" class="btn btn-link btn-sm p-0 ml-2 text-danger" id="reg-file-remove-${suf}">Quitar</button>
    `;
    const removeBtn = document.getElementById(`reg-file-remove-${suf}`);
    if (removeBtn) removeBtn.addEventListener("click", () => {
      fileInput.value = "";
      infoEl.innerHTML = "";
    });
  }
}

// ─── Borrador local y prevención de pérdida de datos (OA-089, OR-089, OR-090) ─

let _submitDirty = { archivo: false, rrhh: false };

function _submitDraftKey(suf) {
  const isArchivo = isArchivoModule();
  return `ds_draft_${isArchivo ? "archivo" : "rrhh"}_${suf}`;
}

function _submitDraftFieldIds(suf, isArchivo) {
  const base = isArchivo
    ? ["reg-title","reg-author","reg-doc-type","reg-secundario","reg-fecha","reg-descriptores",
       "reg-resumen","reg-location","reg-folio","reg-soporte","reg-paginas","reg-vencimiento",
       "reg-personas-archivo"]
    : ["reg-nombres","reg-apellidos","reg-cedula","reg-rif","reg-cargo","reg-personas",
       "reg-doc-type","reg-depto","reg-estado","reg-fecha","reg-location","reg-jubilacion",
       "reg-pension","reg-foto","reg-nacimiento","reg-sexo","reg-nivel"];
  return base.map(id => `${id}-${suf}`);
}

function _markSubmitDirty(suf) {
  const isArchivo = isArchivoModule();
  _submitDirty[isArchivo ? "archivo" : "rrhh"] = true;
  _saveSubmitDraft(suf);
}

function _saveSubmitDraft(suf) {
  const isArchivo = isArchivoModule();
  const ids = _submitDraftFieldIds(suf, isArchivo);
  const data = {};
  let hasValue = false;
  ids.forEach(id => {
    const el = document.getElementById(id);
    if (el && el.value) { data[id] = el.value; hasValue = true; }
  });
  try {
    if (hasValue) localStorage.setItem(_submitDraftKey(suf), JSON.stringify({ ts: Date.now(), data }));
    else localStorage.removeItem(_submitDraftKey(suf));
  } catch {
    // localStorage puede fallar en modo privado; el borrador es una comodidad, no un requisito.
  }
}

function _restoreSubmitDraft(suf) {
  const hintEl = document.getElementById(`reg-draft-hint-${suf}`);
  let saved;
  try { saved = JSON.parse(localStorage.getItem(_submitDraftKey(suf)) || "null"); } catch { saved = null; }
  if (!saved || !saved.data) return;

  Object.entries(saved.data).forEach(([id, value]) => {
    const el = document.getElementById(id);
    if (el) el.value = value;
  });
  const isArchivo = isArchivoModule();
  _submitDirty[isArchivo ? "archivo" : "rrhh"] = true;

  if (hintEl) {
    const cuando = new Date(saved.ts).toLocaleString("es-VE", { hour: "2-digit", minute: "2-digit", day: "2-digit", month: "2-digit" });
    hintEl.innerHTML = `<i class="fas fa-history mr-1"></i>Se recuperó un borrador guardado el ${cuando}.
      <button type="button" class="btn btn-link btn-sm p-0 ml-1 text-danger" id="reg-draft-discard-${suf}">Descartar</button>`;
    const discardBtn = document.getElementById(`reg-draft-discard-${suf}`);
    if (discardBtn) discardBtn.addEventListener("click", () => {
      localStorage.removeItem(_submitDraftKey(suf));
      _submitDirty[isArchivo ? "archivo" : "rrhh"] = false;
      renderDynamicSubmitFields();
    });
  }
}

function _clearSubmitDraft(suf) {
  localStorage.removeItem(_submitDraftKey(suf));
  const isArchivo = isArchivoModule();
  _submitDirty[isArchivo ? "archivo" : "rrhh"] = false;
}

// OR-090: aviso al cerrar/recargar el navegador con datos sin guardar.
window.addEventListener("beforeunload", e => {
  if (_submitDirty.archivo || _submitDirty.rrhh) {
    e.preventDefault();
    e.returnValue = "";
    return "";
  }
});

async function loadRecentSubmissions() {
  const suf         = adminSuffixFromTab();
  const containerEl = document.getElementById(`recent_submissions-${suf}`);
  if (!containerEl) return;
  const isArchivo = isArchivoModule();

  containerEl.innerHTML = `<li class="text-center p-2"><i class="fas fa-spinner fa-spin text-muted"></i></li>`;

  try {
    const modulo = isArchivo ? "Archivo" : "RRHH";
    const data = await apiFetchJSON(`${API_BASE}/api/admin/list_all?modulo=${modulo}&page=1&per_page=5`);
    const records = data.records || [];

    if (records.length === 0) {
      containerEl.innerHTML = `<li class="text-muted text-center p-2">Sin ingresos previos.</li>`;
      return;
    }

    const statusBadge = s => {
      const icons = { aprobado: "fa-check", revision: "fa-clock", draft: "fa-pencil-alt", rechazado: "fa-times" };
      const cls   = { aprobado: "badge-success", revision: "badge-warning text-dark", draft: "badge-secondary", rechazado: "badge-danger" };
      const label = { aprobado: "Aprobado", revision: "Revisión", draft: "Borrador", rechazado: "Rechazado" };
      const st = s || "aprobado";
      return `<span class="badge ${cls[st] || 'badge-secondary'} ds-status-badge" style="font-size:0.65rem;" title="${label[st] || st}"><i class="fas ${icons[st] || 'fa-circle'} mr-1"></i>${label[st] || st}</span>`;
    };

    containerEl.innerHTML = records.map(item => {
      const title = isArchivo ? item.titulo : item.empleado;
      return `<li class="d-flex align-items-center mb-2 p-2 border rounded bg-light">
        <i class="fas fa-file-alt mr-2 text-primary" style="font-size:1.15rem;flex-shrink:0;"></i>
        <div style="overflow:hidden;flex-grow:1;">
          <div style="overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">
            <strong style="font-size:0.82rem;" class="text-dark">${escHtml(title || "Sin título")}</strong>
          </div>
          <div class="d-flex align-items-center gap-1">
            <span class="text-muted" style="font-size:0.70rem;">${escHtml(item.doc_type || "")}</span>
            ${statusBadge(item.status)}
          </div>
        </div>
      </li>`;
    }).join("");
  } catch {
    containerEl.innerHTML = `<li class="text-muted text-center p-2">Error cargando ingresos recientes.</li>`;
  }
}

// ─── Subida con progreso real y cancelación (OA-093, OR-095) ──────────────────
// `apiFetchJSON` usa `fetch`, que no ofrece progreso de subida; para eso hace
// falta `XMLHttpRequest`. Se mantiene aparte para no tocar `app.js` (fuera de
// este carril).
function _uploadFileWithProgress(url, formData, suf) {
  const progWrap = document.getElementById(`reg-upload-progress-${suf}`);
  const bar      = document.getElementById(`reg-upload-bar-${suf}`);
  const pctEl    = document.getElementById(`reg-upload-pct-${suf}`);
  const cancelBtn = document.getElementById(`reg-upload-cancel-${suf}`);

  return new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.open("POST", url, true);
    xhr.withCredentials = true;

    if (progWrap) progWrap.classList.remove("d-none");

    xhr.upload.onprogress = evt => {
      if (!evt.lengthComputable) return;
      const pct = Math.round((evt.loaded / evt.total) * 100);
      if (bar) bar.style.width = `${pct}%`;
      if (pctEl) pctEl.textContent = `${pct}% (${(evt.loaded / 1048576).toFixed(1)} / ${(evt.total / 1048576).toFixed(1)} MB)`;
    };

    xhr.onload = () => {
      if (progWrap) progWrap.classList.add("d-none");
      if (xhr.status >= 200 && xhr.status < 300) {
        try { resolve(JSON.parse(xhr.responseText)); }
        catch { reject(new Error("Respuesta inválida del servidor al subir el archivo.")); }
      } else {
        let detail = `Error ${xhr.status} al subir el archivo.`;
        try { detail = JSON.parse(xhr.responseText).detail || detail; } catch {}
        reject(new Error(detail));
      }
    };
    xhr.onerror = () => { if (progWrap) progWrap.classList.add("d-none"); reject(new Error("Error de red al subir el archivo.")); };
    xhr.onabort = () => { if (progWrap) progWrap.classList.add("d-none"); reject(new Error("__CANCELLED__")); };

    if (cancelBtn) cancelBtn.onclick = () => xhr.abort();

    xhr.send(formData);
  });
}

async function handleNewSubmission(e) {
  e.preventDefault();
  const suf       = adminSuffixFromTab();
  const isArchivo = isArchivoModule();

  function val(id) { return (document.getElementById(id)?.value || "").trim(); }

  // Validación en el borde del sistema (entrada del usuario): una sola lista,
  // recorrida entera, con foco en el primer campo que falle (OA-084, OA-086,
  // OR-087, OR-088).
  const requiredList = SUBMIT_REQUIRED_FIELDS[isArchivo ? "archivo" : "rrhh"];
  let firstInvalid = null;
  requiredList.forEach(f => {
    const err = _validateSubmitField(suf, f.id, isArchivo);
    _setSubmitFieldError(suf, f.id, err);
    if (err && !firstInvalid) firstInvalid = f.id;
  });
  // Campos opcionales con validación propia (RIF, coherencia de fechas).
  ["reg-rif", "reg-nacimiento"].forEach(fieldId => {
    const err = _validateSubmitField(suf, fieldId, isArchivo);
    _setSubmitFieldError(suf, fieldId, err);
    if (err && !firstInvalid) firstInvalid = fieldId;
  });

  if (firstInvalid) {
    const { input } = _submitFieldEls(suf, firstInvalid);
    if (input) { input.focus(); input.scrollIntoView({ behavior: "smooth", block: "center" }); }
    showToast("Revisa los campos marcados en rojo.", "warning");
    return;
  }

  const submitBtn = document.getElementById(`btn_submit_workspace-${suf}`);
  const origLabel = submitBtn?.innerHTML;
  if (submitBtn) { submitBtn.disabled = true; submitBtn.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i> Guardando...'; }

  const payload = {
    modulo:    state.user.modulo,
    usuario:   state.user.username,
    doc_type:  val(`reg-doc-type-${suf}`),
    fecha:     val(`reg-fecha-${suf}`),
    ubicacion: val(`reg-location-${suf}`)
  };

  if (isArchivo) {
    payload.titulo              = val(`reg-title-${suf}`);
    payload.autor               = val(`reg-author-${suf}`);
    payload.resumen             = val(`reg-resumen-${suf}`);
    payload.tesauro_secundario  = val(`reg-secundario-${suf}`);
    payload.descriptores_libres = val(`reg-descriptores-${suf}`);
    payload.numero_folio          = val(`reg-folio-${suf}`) || null;
    payload.soporte               = val(`reg-soporte-${suf}`) || "Físico";
    payload.personas_relacionadas = val(`reg-personas-archivo-${suf}`) || null;
    const venc = val(`reg-vencimiento-${suf}`);
    if (venc) payload.fecha_vencimiento = venc;
    const pags = parseInt(document.getElementById(`reg-paginas-${suf}`)?.value || "");
    if (!isNaN(pags) && pags > 0) payload.numero_paginas = pags;
  } else {
    const nombres   = val(`reg-nombres-${suf}`);
    const apellidos = val(`reg-apellidos-${suf}`);
    payload.nombres               = nombres;
    payload.apellidos             = apellidos;
    payload.empleado              = `${nombres} ${apellidos}`.trim();
    payload.cedula                = val(`reg-cedula-${suf}`);
    payload.personas_relacionadas = val(`reg-personas-${suf}`);
    payload.departamento          = val(`reg-depto-${suf}`);
    payload.estado                = val(`reg-estado-${suf}`);
    payload.rif                   = val(`reg-rif-${suf}`);
    payload.cargo                 = val(`reg-cargo-${suf}`);
    payload.fecha_jubilacion      = val(`reg-jubilacion-${suf}`);
    payload.fecha_pension         = val(`reg-pension-${suf}`);
    payload.foto_url              = val(`reg-foto-${suf}`);
    payload.fecha_nacimiento      = val(`reg-nacimiento-${suf}`) || null;
    payload.sexo                  = val(`reg-sexo-${suf}`) || null;
    payload.nivel_educativo       = val(`reg-nivel-${suf}`) || null;
  }

  const fileInput = document.getElementById(`file_upload-${suf}`);
  const file = fileInput?.files?.[0];
  let fileUploadFailed = false;

  try {
    // Subir archivo digitalizado (si se seleccionó uno) antes de crear el registro.
    if (file) {
      if (submitBtn) submitBtn.innerHTML = '<i class="fas fa-cloud-upload-alt fa-fade mr-1"></i> Subiendo archivo...';
      const fd = new FormData();
      fd.append("file", file);
      fd.append("modulo", state.user.modulo.toLowerCase());
      fd.append("usuario", state.user.username);
      try {
        const upData = await _uploadFileWithProgress(`${API_BASE}/api/admin/upload`, fd, suf);
        payload.file_url = upData.file_url;
      } catch (upErr) {
        // OA-092: si falla la subida, ofrecer guardar sólo los metadatos en vez
        // de perder todo el formulario.
        fileUploadFailed = true;
        if (upErr.message !== "__CANCELLED__") {
          const seguir = window.confirm(
            `No se pudo subir el archivo (${upErr.message}).\n\n¿Guardar el registro sin el archivo digital? Podrás adjuntarlo después desde edición.`
          );
          if (!seguir) {
            if (submitBtn) { submitBtn.disabled = false; submitBtn.innerHTML = origLabel; }
            return;
          }
        } else {
          if (submitBtn) { submitBtn.disabled = false; submitBtn.innerHTML = origLabel; }
          showToast("Subida cancelada.", "info");
          return;
        }
      }
      if (submitBtn) submitBtn.innerHTML = '<i class="fas fa-spinner fa-spin mr-1"></i> Guardando...';
    }

    await apiFetchJSON(`${API_BASE}/api/admin/submit`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });

    showToast(
      fileUploadFailed
        ? "Ingreso guardado sin el archivo digital. Adjúntalo luego desde edición."
        : "Ingreso guardado con éxito.",
      "success"
    );

    _clearSubmitDraft(suf);

    // OA-090 / OR-091: quedarse en «Ingresar» en vez de saltar a Resumen; se
    // conservan tipo/clasificación/ubicación (Archivo) o cédula (RRHH) porque
    // se repiten dentro de una misma caja o expediente.
    const keep = isArchivo
      ? { "reg-doc-type": val(`reg-doc-type-${suf}`), "reg-secundario": val(`reg-secundario-${suf}`), "reg-location": val(`reg-location-${suf}`), "reg-soporte": val(`reg-soporte-${suf}`) }
      : { "reg-depto": val(`reg-depto-${suf}`), "reg-location": val(`reg-location-${suf}`) };

    const form = document.getElementById(`admin-submit-form-${suf}`);
    if (form) form.reset();
    renderDynamicSubmitFields();
    Object.entries(keep).forEach(([id, value]) => {
      const el = document.getElementById(`${id}-${suf}`);
      if (el && value) el.value = value;
    });
    _submitDirty[isArchivo ? "archivo" : "rrhh"] = false;

    loadRecentSubmissions();
    loadDynamicChoices();
    // OR-092: no disparar la búsqueda pública completa desde el panel de admin
    // — su resultado no se pinta en ningún sitio de aquí.
  } catch (err) {
    // OA-091: distinguir el motivo real en vez de un genérico "error de conexión".
    const msg = err?.message || "";
    if (/^Error 4\d\d/.test(msg) || /obligator|requerid|inválid|invalid/i.test(msg)) {
      showToast(msg, "error");
      // OR-035: cuando el backend nombra el campo culpable ("cédula",
      // "RIF"...) se marca ese campo en rojo, no sólo el toast que
      // desaparece a los pocos segundos.
      const fieldByKeyword = isArchivo
        ? [[/t[ií]tulo/i, "reg-title"], [/autor/i, "reg-author"], [/fecha/i, "reg-fecha"], [/ubicaci[oó]n/i, "reg-location"]]
        : [[/c[eé]dula/i, "reg-cedula"], [/\brif\b/i, "reg-rif"], [/nombre/i, "reg-nombres"], [/apellido/i, "reg-apellidos"], [/departamento/i, "reg-depto"], [/fecha/i, "reg-fecha"]];
      const hit = fieldByKeyword.find(([re]) => re.test(msg));
      if (hit) _setSubmitFieldError(suf, hit[1], msg);
    } else if (/^Error 413/.test(msg)) {
      showToast("El archivo es demasiado grande para el servidor.", "error");
    } else if (/^Error 5\d\d/.test(msg)) {
      showToast("Error del servidor al registrar el folio. Intenta de nuevo.", "error");
    } else {
      showToast(msg || "Error de conexión al registrar el folio.", "error");
    }
  } finally {
    if (submitBtn) { submitBtn.disabled = false; submitBtn.innerHTML = origLabel; }
  }
}
