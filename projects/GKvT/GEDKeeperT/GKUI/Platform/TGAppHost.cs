/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Text;
using BSLib;
using BSLib.Design.IoC;
using BSLib.Design.MVP;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// Terminal.Gui.
    /// </summary>
    public sealed class TGAppHost : AppHost
    {
        static TGAppHost()
        {
            SetAppSign("GEDKeeperR");
        }

        public TGAppHost()
        {
        }

        public override void Init(string[] args, bool isMDI)
        {
            base.Init(args, isMDI);
        }

        public override IWindow GetActiveWindow()
        {
            throw new NotImplementedException();
        }

        public override IntPtr GetTopWindowHandle()
        {
            throw new NotImplementedException();
        }

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (fRunningForms.Count == 0) {
                Quit();
            }
        }

        public override bool ShowModalX(ICommonDialog form, bool keepModeless = false)
        {
            return false;
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            throw new NotImplementedException();
        }

        public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            throw new NotImplementedException();
        }

        public override void Quit()
        {
            Application.Top.Running = false;
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            throw new NotImplementedException();
        }

        public override bool ExecuteWorkExt(ProgressStart proc, string title)
        {
            throw new NotImplementedException();
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            throw new NotImplementedException();
        }

        public override string SelectFolder(string folderPath)
        {
            throw new NotImplementedException();
        }

        #region KeyLayout functions

        public override int GetKeyLayout()
        {
            return 0;
        }

        public override void SetKeyLayout(int layout)
        {
        }

        public override void SetClipboardText(string text)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for WinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap()
        {
#if NETCOREAPP3_1_OR_GREATER
            // support for legacy encodings
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
#endif

            var appHost = new TGAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, TGStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProviderEx, TGGfxProvider>(LifeCycle.Singleton);

            // dialogs
            ControlsManager.RegisterHandlerType(typeof(MenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuBarItem), typeof(MenuItemHandler));
        }

        #endregion

        public static void Startup(string[] args)
        {
            ConfigureBootstrap();
            CheckPortable(args);
            Logger.Init(GetLogFilename());
            LogSysInfo();

            AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;
        }

        private static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs e)
        {
            // Saving the copy for restoration
            AppHost.Instance.CriticalSave();
            Logger.WriteError("GK.UnhandledExceptionsHandler()", (Exception)e.ExceptionObject);
        }
    }
}
