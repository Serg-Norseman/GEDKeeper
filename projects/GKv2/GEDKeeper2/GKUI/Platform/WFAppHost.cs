/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Security;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;
using GKUI.Forms;
using GKUI.Platform.Handlers;
using GKUI.Themes;

namespace GKUI.Platform
{
    using CommonDialog = Forms.CommonDialog;


    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// WinForms.
    /// </summary>
    [Serializable]
    public sealed class WFAppHost : AppHost
    {
        static WFAppHost()
        {
            SetAppSign("GEDKeeper2");
        }

        public WFAppHost()
        {
        }

        private void OnApplicationExit(object sender, EventArgs e)
        {
            ApplicationExit();
        }

        public override async Task Init(string[] args, bool isMDI)
        {
            await base.Init(args, isMDI);
            Application.ApplicationExit += OnApplicationExit;
        }

        public override void Activate()
        {
            var forms = Application.OpenForms;
            if (forms.Count > 0) {
                forms[forms.Count - 1].Activate();
            }
        }

        public override IForm GetActiveForm()
        {
            return Form.ActiveForm as IForm;
        }

        public override IWindow GetActiveWindow()
        {
            IWindow activeWin = GetActiveForm() as IWindow;

            if (activeWin == null) {
                activeWin = fActiveBase;
            }

            // only for tests!
            if (activeWin == null && fRunningForms.Count > 0) {
                activeWin = fRunningForms[0];
            }

            return activeWin;
        }

        public override IntPtr GetTopWindowHandle()
        {
            var ownerForm = GetActiveWindow() as Form;
            return (ownerForm == null) ? IntPtr.Zero : ownerForm.Handle;
        }

        public override async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            var wfModal = dialog as CommonDialog;
            if (wfModal == null) return false;

            IntPtr mainHandle = GetTopWindowHandle();

            if (keepModeless) {
                foreach (IWindow win in fRunningForms) {
                    if (win is IBaseWindow) {
                        IntPtr handle = ((Form)win).Handle;

#if !MONO
                        PostMessageExt(handle, WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
#endif
                    }
                }
            }

            // for WinForms works better without this
            //UIHelper.CenterFormByParent((Form)dialog, mainHandle);

            wfModal.ShowDialog(owner as IWin32Window);
            return await wfModal.DialogResultTask;
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            Form frm = form as Form;

            if (frm != null) {
#if !MONO
                EnableWindowExt(frm.Handle, value);
#endif
            }
        }

        public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
            var frm = baseWin as Form;
            mf.WinRect = UIHelper.GetFormRect(frm);
            mf.WinState = (WindowState)frm.WindowState;
        }

        public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
            var frm = baseWin as Form;
            UIHelper.RestoreFormRect(frm, mf.WinRect, (FormWindowState)mf.WinState);
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            var result = new WFUITimer(msInterval, elapsedHandler);
            return result;
        }

        public override void Quit()
        {
            Application.Exit();
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            var activeWnd = GetActiveWindow();

            // FIXME: dirty hack
            // In test mode, when a stub is substituted for the real form, 
            // the modal show of the dialog does not block further code execution after ExecuteWork.
            if (TEST_MODE) {
                using (var progressForm = ResolveDialog<IProgressDialog>()) {
                    proc(progressForm);
                }
                return;
            }

            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                try {
                    workerThread.Start(progressForm);

                    ((Form)progressForm).ShowDialog(activeWnd as IWin32Window);
                } catch (Exception ex) {
                    Logger.WriteError("ExecuteWork()", ex);
                }
            }
        }

        public override bool ExecuteWorkExt(ProgressStart proc, string title)
        {
            var activeWnd = GetActiveWindow() as Form;

            using (var progressForm = new ProgressDlg()) {
                progressForm.Text = title;

                var threadWorker = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                DialogResult dialogResult = DialogResult.Abort;
                try {
                    threadWorker.Start(progressForm);

                    dialogResult = progressForm.ShowDialog(activeWnd);
                } finally {
                    threadWorker.Join();
                }

                if (dialogResult == DialogResult.Abort) {
                    if (progressForm.ThreadError.Message == "") {
                        // Abort means there were file IO errors
                        StdDialogs.ShowAlert("UnkProblem" /*fLangMan.LS(PLS.UnkProblem)*/);
                    } else {
                        // Abort means there were file IO errors
                        StdDialogs.ShowAlert(progressForm.ThreadError.Message);
                    }
                }

                if (dialogResult != DialogResult.OK) {
                    return false;
                }

                return true;
            }
        }

        public override void CloseDependentWindows(IWindow owner)
        {
            var wndArr = Application.OpenForms;
            for (int i = wndArr.Count - 1; i >= 0; i--) {
                Form wnd = wndArr[i];
                if (wnd is IWindowDependent && ((IWindowDependent)wnd).OwnerWindow == owner) {
                    wnd.Close();
                }
            }
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            var activeForm = GetActiveWindow() as Form;
            var screen = Screen.FromRectangle(activeForm.Bounds);
            return UIHelper.Rt2Rt(screen.WorkingArea);
        }

        public override void SetWindowBounds(IWindow window, ExtRect bounds)
        {
            var form = window as Form;
            if (form == null) return;

            form.Bounds = UIHelper.Rt2Rt(bounds);
        }

        public override void WidgetLocate(IWidgetForm view, WidgetLocation location)
        {
            var form = view as Form;
            if (form != null) {
                var loc = WidgetLocate(UIHelper.Rt2Rt(form.Bounds), location);
                form.Location = new System.Drawing.Point(loc.X, loc.Y);
            }
        }

        public override string SelectFolder(string folderPath)
        {
            using (var fldDlg = new FolderBrowserDialog()) {
                fldDlg.SelectedPath = folderPath;
                return (fldDlg.ShowDialog() != DialogResult.OK) ? string.Empty : fldDlg.SelectedPath;
            }
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            bool result = false;

            switch (feature) {
                case Feature.GridCellFormat:
                    result = true;
                    break;

                case Feature.InternetProxy:
                    // Not used yet, obsolete
                    result = false;
                    break;

                case Feature.MediaPlayer:
                    result = true;
                    break;

                case Feature.RecentFilesLoad:
                    // In the SDI interface, it is not clear how to implement it correctly
                    result = true;
                    break;

                case Feature.Themes:
                    // since v2.23.0, only WinForms-based implementation, on Windows OS
#if !MONO
                    result = true;
#endif
                    break;

                case Feature.OverwritePrompt:
                    result = true;
                    break;
            }

            return result;
        }

        public override void LayoutWindows(WinLayout layout)
        {
            var activeForm = GetActiveWindow() as Form;
            var screen = Screen.FromRectangle(activeForm.Bounds);
            var scrLoc = screen.WorkingArea.Location;
            var scrSize = screen.WorkingArea.Size;

            int frameBorderSize = 0;
            var osType = SysUtils.GetOSType();
            if (osType >= OSType.Windows10) {
                frameBorderSize = 8;
                scrLoc.Offset(-frameBorderSize, 0);
            }

            switch (layout) {
                case WinLayout.Cascade: {
                        Size newSize = new Size(Convert.ToInt32(scrSize.Width * 0.8), Convert.ToInt32(scrSize.Height * 0.8));
                        int i = 0;
                        foreach (IWindow win in fRunningForms) {
                            var form = win as Form;
                            if (form == null || !form.Visible || form.WindowState == FormWindowState.Minimized) {
                                continue;
                            } else if (form.WindowState == FormWindowState.Maximized) {
                                form.WindowState = FormWindowState.Normal;
                            }
                            int l = 22 * i;
                            int t = 22 * i;
                            if (i != 0 && (l + newSize.Width > scrSize.Width || t + newSize.Height > scrSize.Height)) {
                                i = 0;
                                l = 22 * i;
                                t = 22 * i;
                            }
                            form.Location = new Point(scrLoc.X + l, scrLoc.Y + t);
                            form.Size = newSize;
                            i++;
                        }
                        break;
                    }

                case WinLayout.TileHorizontal:
                case WinLayout.TileVertical: {
                        int total = 0;
                        foreach (IWindow win in fRunningForms) {
                            var form = win as Form;
                            if (form == null || !form.Visible || form.WindowState == FormWindowState.Minimized) {
                                continue;
                            } else if (form.WindowState == FormWindowState.Maximized) {
                                form.WindowState = FormWindowState.Normal;
                            }
                            total++;
                        }
                        if (total <= 0)
                            return;

                        Size newSize;
                        Size offset;
                        if (layout == WinLayout.TileHorizontal) {
                            newSize = new Size(scrSize.Width, (scrSize.Height / total));
                            offset = new Size(0, newSize.Height);
                        } else {
                            newSize = new Size((scrSize.Width / total), scrSize.Height);
                            offset = new Size(newSize.Width, 0);
                        }
                        // Correction can only be after calculating the offset
                        newSize = new Size(newSize.Width + frameBorderSize * 2, newSize.Height + frameBorderSize);

                        Point nextLocation = scrLoc;
                        foreach (IWindow win in fRunningForms) {
                            var form = win as Form;
                            if (form == null || !form.Visible || form.WindowState == FormWindowState.Minimized) {
                                continue;
                            }
                            form.Location = nextLocation;
                            form.Size = newSize;
                            nextLocation += offset;
                        }
                        break;
                    }

                case WinLayout.Minimize: {
                        foreach (IWindow win in fRunningForms) {
                            var form = win as Form;
                            if (form != null) {
                                form.WindowState = FormWindowState.Minimized;
                            }
                        }
                        break;
                    }
            }
        }

        #region KeyLayout functions

        public override int GetKeyLayout()
        {
            #if MONO
            // There is a bug in Mono: does not work this CurrentInputLanguage
            return CultureInfo.CurrentUICulture.KeyboardLayoutId;
            #else
            InputLanguage currentLang = InputLanguage.CurrentInputLanguage;
            return currentLang.Culture.KeyboardLayoutId;
            #endif
        }

        public override void SetKeyLayout(int layout)
        {
            try {
                CultureInfo cultureInfo = new CultureInfo(layout);
                InputLanguage currentLang = InputLanguage.FromCulture(cultureInfo);
                InputLanguage.CurrentInputLanguage = currentLang;
            } catch (Exception ex) {
                Logger.WriteError("WFAppHost.SetKeyLayout()", ex);
            }
        }

        public override void SetClipboardText(string text)
        {
            Clipboard.SetDataObject(text);
        }

        #endregion

        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for WinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap()
        {
            var appHost = new WFAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, WFStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, WFGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);
            container.Register<IThemeManager, WFThemeManager>(LifeCycle.Singleton);

            // dialogs
            container.Register<IAboutDlg, AboutDlg>(LifeCycle.Transient);
            container.Register<IAddressEditDlg, AddressEditDlg>(LifeCycle.Transient);
            container.Register<IAssociationEditDlg, AssociationEditDlg>(LifeCycle.Transient);
            container.Register<IBaseWindow, BaseWinSDI>(LifeCycle.Transient);
            container.Register<ICircleChartWin, CircleChartWin>(LifeCycle.Transient);
            container.Register<ICommunicationEditDlg, CommunicationEditDlg>(LifeCycle.Transient);
            container.Register<ICommonFilterDlg, CommonFilterDlg>(LifeCycle.Transient);
            container.Register<IDayTipsDlg, DayTipsDlg>(LifeCycle.Transient);
            container.Register<IEventEditDlg, EventEditDlg>(LifeCycle.Transient);
            container.Register<IFamilyEditDlg, FamilyEditDlg>(LifeCycle.Transient);
            container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            container.Register<IFragmentSearchDlg, TTFamilyGroupsDlg>(LifeCycle.Transient);
            container.Register<IGroupEditDlg, GroupEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageEditDlg, LanguageEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageSelectDlg, LanguageSelectDlg>(LifeCycle.Transient);
            container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            container.Register<ILocationNameEditDlg, LocationNameEditDlg>(LifeCycle.Transient);
            container.Register<ILocationLinkEditDlg, LocationLinkEditDlg>(LifeCycle.Transient);
            container.Register<IMapsViewerWin, MapsViewerWin>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            container.Register<IMediaViewerWin, MediaViewerWin>(LifeCycle.Transient);
            container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlgEx, NoteEditDlgEx>(LifeCycle.Transient);
            container.Register<IOptionsDlg, OptionsDlg>(LifeCycle.Transient);
            container.Register<IOrganizerWin, OrganizerWin>(LifeCycle.Transient);
            container.Register<IParentsEditDlg, ParentsEditDlg>(LifeCycle.Transient);
            container.Register<IPatriarchsSearchDlg, TTPatSearchDlg>(LifeCycle.Transient);
            container.Register<IPatriarchsViewer, PatriarchsViewerWin>(LifeCycle.Transient);
            container.Register<IPersonsFilterDlg, PersonsFilterDlg>(LifeCycle.Transient);
            container.Register<IPlacesManagerDlg, TTPlacesManagerDlg>(LifeCycle.Transient);
            container.Register<IPersonalNameEditDlg, PersonalNameEditDlg>(LifeCycle.Transient);
            container.Register<IPersonEditDlg, PersonEditDlg>(LifeCycle.Transient);
            container.Register<IPortraitSelectDlg, PortraitSelectDlg>(LifeCycle.Transient);
            container.Register<IRecMergeDlg, TTRecMergeDlg>(LifeCycle.Transient);
            container.Register<IRecordSelectDialog, RecordSelectDlg>(LifeCycle.Transient);
            container.Register<IRelationshipCalculatorDlg, RelationshipCalculatorDlg>(LifeCycle.Transient);
            container.Register<IRepositoryEditDlg, RepositoryEditDlg>(LifeCycle.Transient);
            container.Register<IResearchEditDlg, ResearchEditDlg>(LifeCycle.Transient);
            container.Register<ISexCheckDlg, SexCheckDlg>(LifeCycle.Transient);
            container.Register<ISourceCitEditDlg, SourceCitEditDlg>(LifeCycle.Transient);
            container.Register<ISourceEditDlg, SourceEditDlg>(LifeCycle.Transient);
            container.Register<IScriptEditWin, ScriptEditWin>(LifeCycle.Transient);
            container.Register<ISlideshowWin, SlideshowWin>(LifeCycle.Transient);
            container.Register<IStatisticsWin, StatisticsWin>(LifeCycle.Transient);
            container.Register<ITaskEditDlg, TaskEditDlg>(LifeCycle.Transient);
            container.Register<ITreeChartWin, TreeChartWin>(LifeCycle.Transient);
            container.Register<ITreeCheckDlg, TTTreeCheckDlg>(LifeCycle.Transient);
            container.Register<ITreeCompareDlg, TTTreeCompareDlg>(LifeCycle.Transient);
            container.Register<ITreeFilterDlg, TreeFilterDlg>(LifeCycle.Transient);
            container.Register<ITreeMergeDlg, TTTreeMergeDlg>(LifeCycle.Transient);
            container.Register<ITreeSplitDlg, TTTreeSplitDlg>(LifeCycle.Transient);
            container.Register<IUserRefEditDlg, UserRefEditDlg>(LifeCycle.Transient);
            container.Register<IRecordInfoDlg, RecordInfoDlg>(LifeCycle.Transient);
            container.Register<IFARDlg, FindAndReplaceDlg>(LifeCycle.Transient);
            container.Register<IEventDefEditDlg, EventDefEditDlg>(LifeCycle.Transient);
            container.Register<ILocExpertDlg, LocExpertDlg>(LifeCycle.Transient);
            container.Register<IPartialView, PartialView>(LifeCycle.Transient);
            container.Register<ISourceCallNumberEditDlg, SourceCallNumberEditDlg>(LifeCycle.Transient);
            container.Register<IRepositoryCitEditDlg, RepositoryCitEditDlg>(LifeCycle.Transient);

            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(MaskedTextBox), typeof(MaskedTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TextBox), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripMenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripComboBox), typeof(ToolStripComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(RichTextBox), typeof(RichTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TabPage), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(GroupBox), typeof(GroupBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripButton), typeof(ButtonToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripLabel), typeof(LabelToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripDropDownButton), typeof(DropDownToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(Splitter), typeof(SplitterHandler));

            ControlsManager.RegisterHandlerType(typeof(GKTabControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateBox), typeof(DateBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateControl), typeof(DateControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKListView), typeof(ListViewHandler));
        }

        #endregion

        #region NativeMethods

        public const uint WM_USER = 0x0400;
        public const uint WM_KEEPMODELESS = WM_USER + 111;

        #if !MONO

        [SecurityCritical, SuppressUnmanagedCodeSecurity]
        [DllImport("user32.dll", EntryPoint="PostMessage", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool PostMessageExt(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [SecurityCritical, SuppressUnmanagedCodeSecurity]
        [DllImport("user32.dll", EntryPoint="EnableWindow", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool EnableWindowExt(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);

        #endif

        #endregion

        public static void Startup(string[] args)
        {
            ConfigureBootstrap();
            CheckPortable(args);
            Logger.Init(GetLogFilename());
            LogSysInfo();

            Application.ThreadException += ExExceptionHandler;
            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
        }

        private static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
        {
            Logger.WriteError("GK.ExExceptionHandler()", args.Exception);
        }
    }
}
