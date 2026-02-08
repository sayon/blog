// Theme toggle functionality
(function() {
  const root = document.documentElement;

  const toggle = () => {
    const btn = document.querySelector('.theme-toggle .theme-icon');
    const theme = root.getAttribute('data-theme');

    if (theme === 'dark') {
      root.setAttribute('data-theme', 'light');
      if (btn) btn.textContent = '🌙';
      localStorage.setItem('theme', 'light');
    } else {
      root.setAttribute('data-theme', 'dark');
      if (btn) btn.textContent = '☀️';
      localStorage.setItem('theme', 'dark');
    }
  };

  window.toggleTheme = toggle;

  // Initialize theme on page load
  const saved = localStorage.getItem('theme');
  const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  const theme = saved || (prefersDark ? 'dark' : 'light');
  root.setAttribute('data-theme', theme);

  // Set initial icon after DOM loads
  document.addEventListener('DOMContentLoaded', function() {
    const btn = document.querySelector('.theme-toggle .theme-icon');
    if (btn) {
      btn.textContent = theme === 'dark' ? '☀️' : '🌙';
    }
  });
})();
