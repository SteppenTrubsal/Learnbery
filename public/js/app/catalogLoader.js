import { bus } from '../core/bus.js';
import { ajaxClient } from '../core/ajax.js';
import { msgEvent } from '../core/types.js';

class CatalogLoader {
  constructor() {
    this.offset = 0;
    this.limit = 20;
    this.loading = false;
    this.allLoaded = false;
    this.container = null;
  }

  init() {
    // Find the container element for book covers
    this.container = document.getElementById('book-list');
    if (!this.container) return;

    // Load the first batch of books
    this.loadMore();

    // On scroll, load more books when reaching near bottom of page
    window.addEventListener('scroll', () => {
      if (this.loading || this.allLoaded) return;
      const nearBottom = window.innerHeight + window.pageYOffset >= document.body.offsetHeight - 100;
      if (nearBottom) {
        this.loadMore();
      }
    });

    // Handle incoming book list data from the server
    bus.on(msgEvent('books:list'), ({ payload }) => {
      this.loading = false;
      if (!Array.isArray(payload) || payload.length === 0) {
        // No more books to load
        this.allLoaded = true;
        return;
      }
      // Append each book cover as a new card in the grid
      for (const book of payload) {
        const col = document.createElement('article');
        col.className = 'col';
        // Create a card with the cover image
        const card = document.createElement('div');
        card.className = 'card h-100';
        card.setAttribute('onmouseover', `onBookHover('${book.id}', this)`);
        card.setAttribute('onclick', `onBookClick('${book.id}', this)`);
        const img = document.createElement('img');
        img.src = book.cover;
        img.alt = `Обложка книги '${book.title}'`;
        img.className = 'img-fluid';
        card.appendChild(img);
        col.appendChild(card);
        this.container.appendChild(col);
      }
      // Update offset for next batch and check if end of list
      this.offset += payload.length;
      if (payload.length < this.limit) {
        this.allLoaded = true;
      }
    });
  }

  // Helper to load the next page of books
  loadMore() {
    if (this.loading) return;
    this.loading = true;
    ajaxClient.request({
      path: `/books?offset=${this.offset}&limit=${this.limit}`,
      method: 'GET',
      type: 'books:list',
    });
  }
}

export const catalogLoader = new CatalogLoader();
