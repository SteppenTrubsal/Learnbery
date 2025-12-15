// popup.js
import { bus } from '../core/bus.js';
import { BUS_EVENTS, msgEvent } from '../core/types.js';

class BookDetailsPopup {
  constructor() {
    this.popupEl = null;
    this.anchorEl = null;
    this.visible = false;

    this.hoverOnAnchor = false;
    this.hoverOnPopup = false;
    this.hideTimer = null;
  }

  init() {
    // bus.on(BUS_EVENTS.UI.BOOK.CLICK, ({ el }) => {
    //   if (el instanceof HTMLElement) {
    //     this.anchorEl = el;
    //   }
    // });

    bus.on(BUS_EVENTS.UI.BOOK.HOVER, ({ el }) => {
      if (el instanceof HTMLElement) {
        this.anchorEl = el;
        this.hoverOnAnchor = true;
      }
    });

    bus.on(BUS_EVENTS.UI.BOOK.LEAVE, ({ el }) => {
      if (el instanceof HTMLElement && el === this.anchorEl) {
        this.hoverOnAnchor = false;
        this.maybeHide();
      }
    });

    bus.on(msgEvent('book:details'), ({ payload }) => {
      this.show(payload);
    });
  }

  ensurePopupEl() {
    if (this.popupEl) return;

    const popup = document.createElement('div');
    popup.className = 'book-inline-popup';
    popup.style.position = 'absolute';
    popup.style.zIndex = '999';
    popup.style.background = '#ffffff';
    popup.style.color = '#000000';
    popup.style.borderRadius = '8px';
    popup.style.boxShadow = '0 8px 20px rgba(0,0,0,0.25)';
    popup.style.padding = '1rem';
    popup.style.boxSizing = 'border-box';
    popup.style.maxHeight = '80vh';
    popup.style.overflowY = 'auto';

    const close = document.createElement('button');
    close.type = 'button';
    close.innerText = '×';
    close.style.position = 'absolute';
    close.style.top = '0.25rem';
    close.style.right = '0.5rem';
    close.style.border = 'none';
    close.style.background = 'transparent';
    close.style.fontSize = '1.25rem';
    close.style.cursor = 'pointer';

    close.addEventListener('click', () => this.hide());

    popup.addEventListener('mouseenter', () => {
      this.hoverOnPopup = true;
      if (this.hideTimer) {
        clearTimeout(this.hideTimer);
        this.hideTimer = null;
      }
    });

    popup.addEventListener('mouseleave', () => {
      this.hoverOnPopup = false;
      this.maybeHide();
    });

    popup.appendChild(close);

    const content = document.createElement('div');
    content.className = 'book-inline-popup-content';
    content.style.paddingTop = '0.5rem';
    popup.appendChild(content);

    document.body.appendChild(popup);
    this.popupEl = popup;
  }

  show(book) {
    if (!this.anchorEl) return;

    this.ensurePopupEl();

    const content = this.popupEl.querySelector('.book-inline-popup-content');
    if (!content) return;

    const title = book.title || book.name || 'Без названия';

    const authorsArr = Array.isArray(book.authors) ? book.authors : [];
    const author =
      book.author ||
      (authorsArr.length ? authorsArr.join(', ') : 'Неизвестный автор');

    const genresArr = Array.isArray(book.genres) ? book.genres : [];
    const genresText = genresArr.length ? genresArr.join(', ') : '—';

    const year = book.year || '';
    const description = book.description || book.desc || 'Описание отсутствует';

    const downloadUrl = book.downloadUrl || null;

    const downloadBtn = downloadUrl
      ? `<div style="margin-top: 0.75rem;">
          <a class="btn btn-sm btn-primary w-100" href="${escapeHtml(downloadUrl)}">
            Скачать
          </a>
        </div>`
      : '';

    content.innerHTML = `
      <h3 style="margin: 0 0 0.5rem 0;">${escapeHtml(title)}</h3>

      <p style="margin: 0 0 0.25rem 0;">
        <strong>Автор:</strong> ${escapeHtml(author)}
      </p>

      <p style="margin: 0 0 0.25rem 0;">
        <strong>Жанры:</strong> ${escapeHtml(genresText)}
      </p>

      ${
        year
          ? `<p style="margin: 0 0 0.25rem 0;">
               <strong>Год издания:</strong> ${escapeHtml(String(year))}
            </p>`
          : ''
      }

      <p style="margin: 0.5rem 0 0 0;">${escapeHtml(description)}</p>
      ${downloadBtn}
    `;

    this.positionNearAnchor();
    this.popupEl.style.display = 'block';
    this.visible = true;
  }

  maybeHide() {
    if (this.hideTimer) clearTimeout(this.hideTimer);

    this.hideTimer = setTimeout(() => {
      this.hideTimer = null;
      if (!this.hoverOnAnchor && !this.hoverOnPopup) {
        this.hide();
      }
    }, 120);
  }

  hide() {
    if (!this.popupEl) return;
    this.popupEl.style.display = 'none';
    this.visible = false;

    this.hoverOnAnchor = false;
    this.hoverOnPopup = false;

    if (this.hideTimer) {
      clearTimeout(this.hideTimer);
      this.hideTimer = null;
    }
  }

  positionNearAnchor() {
    if (!this.anchorEl || !this.popupEl) return;

    const rect = this.anchorEl.getBoundingClientRect();
    const scrollX = window.scrollX || window.pageXOffset;
    const scrollY = window.scrollY || window.pageYOffset;

    this.popupEl.style.width = rect.width + 'px';

    const gap = 16;
    let left = rect.right + gap + scrollX;
    let top = rect.top + scrollY;

    const popupRect = this.popupEl.getBoundingClientRect();
    const viewportWidth = document.documentElement.clientWidth || window.innerWidth;

    if (left + popupRect.width > viewportWidth - 8) {
      left = rect.left - popupRect.width - gap + scrollX;
    }

    if (top < scrollY + 8) {
      top = scrollY + 8;
    }

    this.popupEl.style.left = `${left}px`;
    this.popupEl.style.top = `${top}px`;
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
