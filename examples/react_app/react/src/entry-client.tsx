/**
 * Client Entry Point
 *
 * Used for:
 * - Level 1 (Static): Full client-side rendering
 * - Level 2 (Hydration): Hydrates server-rendered HTML
 * - Level 3 (SSR): Hydrates fully server-rendered React
 */

import { StrictMode } from 'react';
import { hydrateRoot, createRoot } from 'react-dom/client';
import App from './App';
import './index.css';

const container = document.getElementById('root')!;

// Check if we have server-rendered content (Level 2 or 3)
const hasServerContent = container.innerHTML.trim().length > 0;

if (hasServerContent) {
  // Hydrate existing server-rendered HTML
  console.log('[Kirin] Hydrating server-rendered content');
  hydrateRoot(
    container,
    <StrictMode>
      <App />
    </StrictMode>
  );
} else {
  // Full client-side render (Level 1)
  console.log('[Kirin] Client-side rendering');
  createRoot(container).render(
    <StrictMode>
      <App />
    </StrictMode>
  );
}
