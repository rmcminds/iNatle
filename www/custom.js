// custom.js

function updateLetterWidth() {
  const gameuidoc = document.getElementById('game_ui');
  const guessword = document.getElementById('current_guess');
  const previousword = document.getElementById('previous_guesses');
  
  if (gameuidoc && guessword) {
    const guesswordHeight = Math.max(previousword.firstChild.offsetHeight, guessword.firstChild.offsetHeight);
    gameuidoc.style.setProperty('--guessword-height', `${guesswordHeight}px`);
  }
}

// Create a MutationObserver to watch for changes in the parent
const observer = new MutationObserver((mutations) => {
  mutations.forEach((mutation) => {
    if (mutation.addedNodes.length) {
      updateLetterWidth(); // Call when the target div is added
    }
  });
});

// Start observing the parent element for child node additions
const mycontainer = document.getElementById('mycontainer');
observer.observe(mycontainer, { childList: true, subtree: true });

// Update on window resize
window.addEventListener('resize', updateLetterWidth);

const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
const all_key_ids = [...letters, 'Enter', 'Back'];

document.addEventListener('keydown', function(e) {
  let key = e.code.replace(/^Key/, '');
  if (letters.includes(key)) {
    document.getElementById(key).click();
  } else if (key === 'Enter') {
    document.getElementById('Enter').click();
  } else if (key === 'Backspace') {
    document.getElementById('Back').click();
  }
});

document.addEventListener('touchstart', function(e) {
  if (all_key_ids.includes(e.target.id)) {
    e.target.click();
    e.target.style.pointerEvents = 'none';
    e.preventDefault();
    in_button_touch = true;
  }
});

document.addEventListener('touchend', function(e) {
  all_key_ids.forEach(id => {
    document.getElementById(id).style.pointerEvents = null;
  });
  if (in_button_touch) {
      if (all_key_ids.includes(e.target.id)) {
          // Disable text selection and triggering of click event.
          e.preventDefault();
      }
    in_button_touch = false;
  }
});
