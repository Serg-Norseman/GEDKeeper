/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Linq;
using System.Threading.Tasks;
using GKCore;
using GKUI.Forms;

namespace GKUI.Platform
{
    /// <summary>
    /// Coordinator of the session-wide base context and the current work window
    /// (mobile variant). The context is created once per application session and
    /// outlives its windows; the window is recreated on every entry into the
    /// workspace and destroyed when popped from the navigation stack.
    /// </summary>
    public sealed class AppWorkspace
    {
        private readonly XFAppHost fHost;

        public BaseContext SharedContext { get; private set; }
        public BaseWinSDI CurrentWindow { get; private set; }


        public AppWorkspace(XFAppHost host)
        {
            fHost = host;
        }

        /// <summary>
        /// Returns the current work window; creates the shared context, if it does
        /// not exist yet, and a new window, if it is not on the stack. Entry points:
        /// "new / open / recent base" and the menu item "Records".
        /// </summary>
        public Task<BaseWinSDI> EnsureBaseWindowAsync()
        {
            if (CurrentWindow != null) {
                // OnDisappearing fires while the popped page is still in the stack,
                // so the actual stack membership is the single source of truth (INV-4)
                var navPage = XFAppHost.GetMainPage()?.NavigationPage;
                if (navPage == null || !navPage.Navigation.NavigationStack.Contains(CurrentWindow)) {
                    CurrentWindow.DetachWindow();
                }

                if (CurrentWindow != null) {
                    return Task.FromResult(CurrentWindow);
                }
            }

            if (SharedContext == null) {
                SharedContext = new BaseContext(null);
            }

            return CreateWindowAsync();
        }

        /// <summary>
        /// Ensures the work window exists for an already loaded context
        /// (ensure-policy for tools and exports); returns null when no context
        /// exists yet, so actions without a database remain no-ops.
        /// </summary>
        public Task<BaseWinSDI> EnsureWindowForContextAsync()
        {
            if (SharedContext == null) {
                return Task.FromResult<BaseWinSDI>(null);
            }

            return EnsureBaseWindowAsync();
        }

        public async Task<BaseWinSDI> NewFileAsync()
        {
            var win = await EnsureBaseWindowAsync();
            await win.Controller.NewFile();
            return win;
        }

        public async Task<BaseWinSDI> LoadFileAsync(string fileName)
        {
            var win = await EnsureBaseWindowAsync();
            await AppHost.Instance.LoadBase(win, fileName);
            return win;
        }

        // called by the window itself when it is popped from the navigation stack
        public void OnWindowPopped(BaseWinSDI window)
        {
            if (window == null || CurrentWindow != window) return;

            CurrentWindow = null;
        }

        // application exit: save the list of last bases and dispose the context
        public void Shutdown()
        {
            fHost.SaveLastBases();

            CurrentWindow = null;

            if (SharedContext != null) {
                SharedContext.SetViewer(null);
                SharedContext.Dispose();
                SharedContext = null;
            }
        }

        // a window is always created with 'new' (a repeated push of the same
        // instance after its pop is unreliable in MAUI) and shown via CommonWindow.Show
        private Task<BaseWinSDI> CreateWindowAsync()
        {
            var win = new BaseWinSDI(SharedContext);
            CurrentWindow = win;

            win.Show(true);
            win.Controller.SetMainTitle();
            AppHost.Instance.RestoreWinMRU(win);

            return Task.FromResult(win);
        }
    }
}
