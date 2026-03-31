document.addEventListener("DOMContentLoaded", () => {
    const path = window.location.pathname;
  
    // Supporte éventuellement un site servi sous /docs/
    const base = path.startsWith("/docs/") ? "/docs" : "";
  
    const inEN = path.startsWith(base + "/en/");
    const inFR = path.startsWith(base + "/fr/");
  
    if (!inEN && !inFR) return;
  
    // URL miroir = même chemin après /en/ ou /fr/
    const mirrorPath = inEN
      ? path.replace(new RegExp("^" + base + "/en/"), base + "/fr/")
      : path.replace(new RegExp("^" + base + "/fr/"), base + "/en/");
  
    const mirrorUrl = mirrorPath + window.location.search + window.location.hash;
  
    // On vise le bouton langue unique par son texte visible
    const navAnchors = Array.from(document.querySelectorAll("nav a"));
    const target = navAnchors.find(a => {
      const t = a.textContent.trim().toLowerCase();
      return t === "english" || t === "français";
    });
  
    if (target) target.setAttribute("href", mirrorUrl);
  });  