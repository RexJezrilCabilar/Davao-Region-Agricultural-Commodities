const els = {
  heroPrice: document.getElementById('heroPrice'),
  heroCaption: document.getElementById('heroCaption'),
  checked: document.getElementById('checked'),
  notice: document.getElementById('sampleNotice'),
  statStrip: document.getElementById('statStrip'),
  chart: document.getElementById('chart'),
  tableBody: document.getElementById('tableBody'),
  searchBox: document.getElementById('searchBox'),
  table: document.getElementById('dataTable'),
};

let rows = [];
let sortKey = 'Price_PHP';
let sortDir = 'desc';

const peso = new Intl.NumberFormat('en-PH', { style: 'currency', currency: 'PHP' });
const isNational = (name) => /^philippines$/i.test((name || '').trim());

async function loadJSON(path) {
  const res = await fetch(path, { cache: 'no-store' });
  if (!res.ok) throw new Error(`${path} responded ${res.status}`);
  return res.json();
}

function fmtChecked(iso) {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return iso;
  return d.toLocaleString('en-PH', { dateStyle: 'medium', timeStyle: 'short', timeZone: 'Asia/Manila' }) + ' PHT';
}

function renderHero(data) {
  const national = data.find(d => isNational(d.Geolocation));
  if (national) {
    els.heroPrice.textContent = peso.format(national.Price_PHP);
    els.heroCaption.textContent = `national average, well-milled rice (${national.Month} ${national.Year})`;
  } else {
    const avg = data.reduce((s, d) => s + d.Price_PHP, 0) / data.length;
    els.heroPrice.textContent = peso.format(avg);
    els.heroCaption.textContent = 'average across tracked regions, well-milled rice';
  }
}

function renderStats(data) {
  const regional = data.filter(d => !isNational(d.Geolocation));
  const list = regional.length ? regional : data;
  const max = list.reduce((a, b) => (b.Price_PHP > a.Price_PHP ? b : a));
  const min = list.reduce((a, b) => (b.Price_PHP < a.Price_PHP ? b : a));
  const avg = list.reduce((s, d) => s + d.Price_PHP, 0) / list.length;

  const stats = [
    { value: String(list.length), label: 'regions tracked' },
    { value: peso.format(avg), label: 'average price' },
    { value: peso.format(max.Price_PHP), label: `highest, ${max.Geolocation}` },
    { value: peso.format(min.Price_PHP), label: `lowest, ${min.Geolocation}` },
  ];

  els.statStrip.innerHTML = stats.map(s => `
    <div class="stat">
      <div class="stat-value">${s.value}</div>
      <div class="stat-label">${s.label}</div>
    </div>
  `).join('');
}

function renderChart(data) {
  const sorted = [...data].sort((a, b) => b.Price_PHP - a.Price_PHP);
  const max = sorted[0].Price_PHP;

  els.chart.innerHTML = sorted.map(d => `
    <div class="bar-row">
      <div class="bar-label" title="${d.Geolocation}">${d.Geolocation}</div>
      <div class="bar-track"><div class="bar-fill" data-pct="${(d.Price_PHP / max * 100).toFixed(1)}"></div></div>
      <div class="bar-value">${peso.format(d.Price_PHP)}</div>
    </div>
  `).join('');

  // Animate the bars in on the next frame, once, rather than starting at full width.
  requestAnimationFrame(() => {
    els.chart.querySelectorAll('.bar-fill').forEach(el => {
      el.style.width = el.dataset.pct + '%';
    });
  });
}

function updateSortIndicators() {
  els.table.querySelectorAll('th[data-key]').forEach(th => {
    th.classList.remove('sort-asc', 'sort-desc');
    if (th.dataset.key === sortKey) th.classList.add(sortDir === 'asc' ? 'sort-asc' : 'sort-desc');
  });
}

function renderTable() {
  const q = els.searchBox.value.trim().toLowerCase();
  const view = rows
    .filter(r => r.Geolocation.toLowerCase().includes(q))
    .sort((a, b) => {
      const va = a[sortKey], vb = b[sortKey];
      const cmp = typeof va === 'number' ? va - vb : String(va).localeCompare(String(vb));
      return sortDir === 'asc' ? cmp : -cmp;
    });

  els.tableBody.innerHTML = view.map(r => `
    <tr>
      <td>${r.Geolocation}</td>
      <td class="num">${peso.format(r.Price_PHP)}</td>
      <td>${r.Year}</td>
      <td>${r.Month}</td>
    </tr>
  `).join('');

  updateSortIndicators();
}

function wireControls() {
  els.table.querySelectorAll('th[data-key]').forEach(th => {
    const toggle = () => {
      if (sortKey === th.dataset.key) {
        sortDir = sortDir === 'asc' ? 'desc' : 'asc';
      } else {
        sortKey = th.dataset.key;
        sortDir = 'asc';
      }
      renderTable();
    };
    th.addEventListener('click', toggle);
    th.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggle(); }
    });
  });
  els.searchBox.addEventListener('input', renderTable);
}

async function init() {
  wireControls();

  try {
    const meta = await loadJSON('./data/meta.json');
    els.checked.textContent = `Checked ${fmtChecked(meta.last_updated_utc)}`;
    if (meta.sample_data) els.notice.hidden = false;
  } catch {
    els.checked.textContent = 'No update history yet';
  }

  try {
    rows = await loadJSON('./data/latest_produce_prices.json');
    if (!Array.isArray(rows) || !rows.length) throw new Error('empty dataset');
    renderHero(rows);
    renderStats(rows);
    renderChart(rows);
    renderTable();
  } catch (err) {
    els.heroPrice.textContent = '—';
    els.heroCaption.textContent = 'no data yet';
    els.chart.innerHTML = '<p class="empty">Nothing to show yet. Once the workflow runs for the first time, prices will appear here.</p>';
  }
}

init();
