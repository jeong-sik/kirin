import { useState, useEffect } from 'react';

interface User {
  id: number;
  name: string;
  email: string;
}

interface AppProps {
  currentUser?: User;
  serverTime?: string;
}

// Access initial data from server (Level 2 hydration)
declare global {
  interface Window {
    __INITIAL_DATA__?: {
      currentUser?: User;
      path?: string;
    };
  }
}

function App({ currentUser: ssrUser, serverTime }: AppProps) {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [currentUser, setCurrentUser] = useState<User | undefined>(
    ssrUser || window.__INITIAL_DATA__?.currentUser
  );

  useEffect(() => {
    fetch('/api/users')
      .then(res => res.json())
      .then(data => {
        setUsers(data);
        setLoading(false);
      })
      .catch(err => {
        console.error('Failed to fetch users:', err);
        setLoading(false);
      });
  }, []);

  return (
    <div className="app">
      <header>
        <h1>Kirin + React</h1>
        <p>OCaml 5.x Eio-native Web Framework with React SSR</p>
        {serverTime && (
          <p className="server-time">Server rendered at: {serverTime}</p>
        )}
      </header>

      <main>
        {currentUser && (
          <section className="current-user">
            <h2>Welcome, {currentUser.name}!</h2>
            <p>Email: {currentUser.email}</p>
          </section>
        )}

        <section className="users">
          <h2>Users</h2>
          {loading ? (
            <p>Loading...</p>
          ) : (
            <ul>
              {users.map(user => (
                <li key={user.id}>
                  <strong>{user.name}</strong> - {user.email}
                </li>
              ))}
            </ul>
          )}
        </section>

        <section className="info">
          <h2>Integration Levels</h2>
          <ul>
            <li><strong>Level 1 (Static)</strong>: Vite build served by Kirin</li>
            <li><strong>Level 2 (Hydration)</strong>: Server HTML shell + client hydration</li>
            <li><strong>Level 3 (SSR)</strong>: Full server-side rendering with Node.js</li>
          </ul>
        </section>
      </main>

      <footer>
        <p>Built with Kirin (OCaml) + React + Vite</p>
      </footer>
    </div>
  );
}

export default App;
