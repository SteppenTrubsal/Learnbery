// popup.js
import { bus } from './bus.js';
import { msgEvent } from './types.js';

class BookDetailsPopup {
  constructor() {
    this.container = null;
  }

  init() {
    bus.on(msgEvent('book:details'), ({ payload }) => {
      this.show(payload);
    });
  }

  createContainer() {
    if (this.container) return;

    const overlay = document.createElement('div');
    overlay.className = 'book-popup-overlay';
    // Можно потом стилизовать через CSS
    overlay.style.position = 'fixed';
    overlay.style.inset = '0';
    overlay.style.background = 'rgba(0, 0, 0, 0.6)';
    overlay.style.display = 'flex';
    overlay.style.alignItems = 'center';
    overlay.style.justifyContent = 'center';
    overlay.style.zIndex = '9999';

    const modal = document.createElement('div');
    modal.className = 'book-popup-modal';
    modal.style.background = '#fff';
    modal.style.color = '#000';
    modal.style.maxWidth = '600px';
    modal.style.width = '90%';
    modal.style.borderRadius = '8px';
    modal.style.padding = '1.5rem';
    modal.style.boxShadow = '0 10px 30px rgba(0,0,0,0.4)';
    modal.style.position = 'relative';

    const closeBtn = document.createElement('button');
    closeBtn.type = 'button';
    closeBtn.innerText = '×';
    closeBtn.style.position = 'absolute';
    closeBtn.style.top = '0.5rem';
    closeBtn.style.right = '0.75rem';
    closeBtn.style.border = 'none';
    closeBtn.style.background = 'transparent';
    closeBtn.style.fontSize = '1.5rem';
    closeBtn.style.cursor = 'pointer';

    closeBtn.addEventListener('click', () => {
      this.hide();
    });

    modal.appendChild(closeBtn);

    const content = document.createElement('div');
    content.className = 'book-popup-content';
    modal.appendChild(content);

    overlay.appendChild(modal);
    document.body.appendChild(overlay);

    this.container = overlay;
  }

  show(book) {
    this.createContainer();

    const content = this.container.querySelector('.book-popup-content');
    if (!content) return;

    const title = book.title || book.name || 'Без названия';
    const author = book.author || 'Неизвестный автор';
    const year = book.year || '';
    const description = book.description || book.desc || 'Описание отсутствует';

    content.innerHTML = `
      <h2 style="margin-top: 0;">${escapeHtml(title)}</h2>
      <p><strong>Автор:</strong> ${escapeHtml(author)}</p>
      ${year ? `<p><strong>Год издания:</strong> ${escapeHtml(String(year))}</p>` : ''}
      <p>${escapeHtml(description)}</p>
    `;

    this.container.style.display = 'flex';
  }

  hide() {
    if (this.container) {
      this.container.style.display = 'none';
    }
  }
}

function escapeHtml(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}

export const bookDetailsPopup = new BookDetailsPopup();
