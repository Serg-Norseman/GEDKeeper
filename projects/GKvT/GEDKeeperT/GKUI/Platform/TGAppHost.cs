/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Globalization;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Utilities;
using GKCore.Validation;
using GKUI.Components;
using GKUI.Forms;
using GKUI.Platform.Handlers;
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

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (fRunningForms.Count == 0) {
                Quit();
            }
        }

        public override IForm GetActiveForm()
        {
            throw new NotImplementedException();
        }

        public override IWindow GetActiveWindow()
        {
            return (fRunningForms != null && fRunningForms.Count > 0) ? fRunningForms[0] : null;
        }

        public override IntPtr GetTopWindowHandle()
        {
            throw new NotImplementedException();
        }

        public override async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            var tgDlg = dialog as CommonDialog;
            Application.Run(tgDlg);
            return (tgDlg.DialogResult == DialogResult.Ok);
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            // not supported
        }

        public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
            // not supported
        }

        public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
            // not supported
        }

        public override GKCore.ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            return new TGTimer(msInterval, elapsedHandler);
        }

        public override void Quit()
        {
            Application.Top.Running = false;
        }

        public override bool ExecuteWork(ProgressStart proc, string title = "")
        {
            //var activeWnd = GetActiveWindow() as IWin32Window;

            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                var progForm = progressForm as CommonDialog;

                // In test mode, when a stub is substituted for the real form, 
                // the modal show of the dialog does not block further code execution after ExecuteWork.
                if (TEST_MODE || progForm == null) {
                    proc(progressForm);
                    return true;
                }

                if (!string.IsNullOrEmpty(title))
                    progressForm.SetTitle(title);

                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                Application.RunState rs;
                DialogResult dialogResult = DialogResult.Abort;
                try {
                    workerThread.Start(progressForm);

                    ///*dialogResult =*/ Application.Run(progForm);

                    rs = Application.Begin(progForm);

                    Application.MainLoop.AddTimeout(TimeSpan.FromMilliseconds(10), x => true);
                } finally {
                    workerThread.Join();
                }

                Application.End(rs);

                /*if (dialogResult == DialogResult.Abort) {
                    if (progressForm.ThreadError.Message == "") {
                        // Abort means there were file IO errors
                        StdDialogs.ShowAlert("UnkProblem");
                    } else {
                        // Abort means there were file IO errors
                        StdDialogs.ShowAlert(progressForm.ThreadError.Message);
                    }
                }

                if (dialogResult != DialogResult.OK) {
                    return false;
                }*/

                return true;
            }
        }

        public override void CloseDependentWindows(IWindow owner)
        {
            throw new NotImplementedException();
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            // not supported
            return ExtRect.Empty;
        }

        public override void SetWindowBounds(IWindow window, ExtRect bounds)
        {
            throw new NotImplementedException();
        }

        public override void WidgetLocate(IWidgetForm view, WidgetLocation location)
        {
            var form = view as Window;
            if (form != null) {
                //var loc = WidgetLocate(UIHelper.Rt2Rt(form.Bounds), location);
                //form.Location = new Point(loc.X, loc.Y);
            }
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            bool result = false;

            switch (feature) {
                case Feature.GridCellFormat:
                    result = false;
                    break;

                case Feature.InternetProxy:
                    result = false;
                    break;

                case Feature.MediaPlayer:
                    result = false;
                    break;

                case Feature.RecentFilesLoad:
                    // In the SDI interface, it is not clear how to implement it correctly
                    result = true;
                    break;

                case Feature.Themes:
                    result = false;
                    break;

                case Feature.OverwritePrompt:
                    result = false;
                    break;

                case Feature.PrintPreview:
                    result = false;
                    break;

                case Feature.Graphics:
                    result = false;
                    break;

                case Feature.DesktopV2:
                case Feature.DesktopV3:
                case Feature.Mobile:
                    result = false;
                    break;
            }

            return result;
        }

        public override void LayoutWindows(WinLayout layout)
        {
            // not supported
        }

        public override void Invoke(Action action)
        {
            action();
        }

        #region KeyLayout functions

        public override int GetKeyLayout()
        {
            // InputLanguage only exists in WinForms
            return CultureInfo.CurrentUICulture.KeyboardLayoutId;
        }

        public override void SetKeyLayout(int layout)
        {
            try {
                CultureInfo.DefaultThreadCurrentUICulture = new CultureInfo(layout);
            } catch (Exception ex) {
                Logger.WriteError("TGAppHost.SetKeyLayout()", ex);
            }
        }

        public override void SetClipboardText(string text)
        {
            throw new NotImplementedException();
        }

        public override void SetClipboardImage(object image)
        {
            // not supported
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
            var container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();
            ValidationFactory.InitGDMValidators();

            // controls and other
            container.Register<IStdDialogs, TGStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, TGGfxProvider>(LifeCycle.Singleton);

            // dialogs
            container.Register<IAboutDlg, AboutDlg>(LifeCycle.Transient);
            container.Register<IChronicleWin, ChronicleWin>(LifeCycle.Transient);
            // ICircleChartWin - not
            container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            // ILanguageSelectDlg - not
            // ILocExpertDlg - not
            // IMapsViewerWin - not
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            // INoteEditDlgEx - not
            container.Register<IOrganizerWin, OrganizerWin>(LifeCycle.Transient);
            // IPartialView - not
            // IPatriarchsViewer - not
            // IPortraitSelectDlg - not
            container.Register<ITreeChartWin, TreeChartWin>(LifeCycle.Transient);
            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);
            // ISlideshowWin - not

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuBarItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(TabView), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TabView.Tab), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(TextField), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TextView), typeof(TextAreaHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripButton), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripDropDownButton), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripMenuItem), typeof(MenuItemHandler));
        }

        #endregion

        public static void Startup(string[] args)
        {
            ConfigureBootstrap();
            CheckPortable(args);
            Logger.Init(GetLogFilename());
            LogSysInfo();
        }
    }
}
