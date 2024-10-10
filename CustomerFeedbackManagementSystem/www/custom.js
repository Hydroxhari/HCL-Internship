function toggleLogoutMenu() {
    const logoutMenu = document.querySelector('.logout-menu');
    logoutMenu.style.display = logoutMenu.style.display === 'block' ? 'none' : 'block';
}

function showNotification(message, type) {
    const notification = document.createElement('div');
    notification.className = `notification ${type}`;
    notification.textContent = message;

    document.body.appendChild(notification);

    setTimeout(() => {
        notification.remove();
    }, 3000);
}

const profileButton = document.querySelector('.profile');
if (profileButton) {
    profileButton.addEventListener('click', toggleLogoutMenu);
}

const feedbackForm = document.querySelector('.feedback-container form');
if (feedbackForm) {
    feedbackForm.addEventListener('submit', function(event) {
        event.preventDefault();

        const feedback = document.querySelector('textarea').value;
        const rating = document.querySelector('input[type="range"]').value;

        fetch('/submit-feedback', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ feedback, rating }),
        })
        .then(response => response.json())
        .then(data => {
            if (data.success) {
                showNotification('Feedback submitted successfully!', 'success');
                feedbackForm.reset();
            } else {
                showNotification('Error submitting feedback. Please try again.', 'error');
            }
        })
        .catch(error => {
            showNotification('An error occurred. Please try again.', 'error');
        });
    });
}

const logoutButton = document.querySelector('.logout-menu button');
if (logoutButton) {
    logoutButton.addEventListener('click', function() {
        fetch('/logout', { method: 'POST' })
        .then(response => {
            if (response.ok) {
                window.location.href = '/login';
            }
        });
    });
}
