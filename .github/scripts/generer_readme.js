// Génère README.md à partir de progression_bts.html (même contenu, même ordre).
// Usage, depuis la racine du dépôt : node .github/scripts/generer_readme.js
// Ne modifie pas README.md à la main : modifie progression_bts.html puis relance ce script.
'use strict';
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..', '..');
const REPO = 'https://github.com/sjaubert/bts-maths-progression';
const html = fs.readFileSync(path.join(ROOT, 'progression_bts.html'), 'utf8');

// 1. Extraction des objets `data` et `resourceMap` du script de la page
const js = html.slice(html.indexOf('const data = ['), html.indexOf('function renderNav'));
const { data, resourceMap } = new Function(js + '\nreturn { data, resourceMap };')();

// 2. Outils
const enc = p => p.split('/').map(encodeURIComponent).join('/');
const label = f => f.replace(/\.pdf$/i, '').replace(/_+/g, ' ');
const bare = code => code.replace(/[\[\]]/g, '');
const esc = s => String(s).replace(/\|/g, '\\|');

// 3. Construction du Markdown
const L = [];
L.push('# Progression Mathématiques BTS');
L.push('');
L.push('Pôle Formation UIMM Centre-Val de Loire. Programme de mathématiques des BTS industriels, organisé en modules.');
L.push('');
L.push('> Version interactive : [progression_bts.html](progression_bts.html).  ');
L.push('> Ce fichier est généré automatiquement à partir de `progression_bts.html` par `.github/scripts/generer_readme.js`. Ne pas le modifier à la main.');
L.push('');
L.push('## Sommaire');
L.push('');
L.push('| Semestre | Module | Intitulé |');
L.push('|---|---|---|');
for (const sem of data) for (const mod of sem.modules) {
  const c = bare(mod.code);
  L.push(`| ${esc(sem.semester)} | [${c}](#${c.toLowerCase()}) | ${esc(mod.title)} |`);
}
L.push('');

for (const sem of data) {
  L.push('---');
  L.push('');
  L.push(`## ${sem.semester}`);
  L.push('');
  for (const mod of sem.modules) {
    const c = bare(mod.code);
    L.push(`<a id="${c.toLowerCase()}"></a>`);
    L.push('');
    L.push(`### ${c} · ${mod.title}`);
    L.push('');
    L.push('**Contenus**');
    L.push('');
    for (const x of mod.content) L.push(`- ${x}`);
    L.push('');
    L.push('**Capacités attendues**');
    L.push('');
    for (const x of mod.capabilities) L.push(`- ${x}`);
    L.push('');
    if (mod.comments) { L.push(`> **Commentaires :** ${mod.comments}`); L.push(''); }
    const res = resourceMap[mod.code] || [];
    if (res.length) {
      L.push(`**Ressources** ([ouvrir le dossier ${c}](${REPO}/tree/main/${c}))`);
      L.push('');
      for (const f of res) L.push(`- [${label(f)}](${enc(c + '/' + f)})`);
    } else {
      L.push("*Aucune ressource liée : le dossier de ce module n'existe pas encore.*");
    }
    L.push('');
    L.push('[Retour au sommaire](#sommaire)');
    L.push('');
  }
}

const OUT = process.argv[2] ? path.resolve(process.argv[2]) : path.join(ROOT, 'README.md');
fs.writeFileSync(OUT, L.join('\n') + '\n', 'utf8');
const n = data.reduce((a, s) => a + s.modules.length, 0);
console.log(`${path.basename(OUT)} généré : ${data.length} semestres, ${n} modules.`);
