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

#pragma warning disable CS0612, CS0618

using System;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
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
    using FormWindowState = Eto.Forms.WindowState;

    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// EtoForms.
    /// </summary>
    public sealed class EtoAppHost : AppHost
    {
        static EtoAppHost()
        {
            SetAppSign("GEDKeeper3e");
        }

        public EtoAppHost()
        {
            InitCommonStyles();
            InitPlatformStyles();
        }

        private static void InitCommonStyles()
        {
            Eto.Style.Add<TableLayout>("paddedTable", table => {
                table.Padding = new Padding(8);
                table.Spacing = new Size(4, 4);
            });

            Eto.Style.Add<TableLayout>("paddedTable8", table => {
                table.Padding = new Padding(8);
                table.Spacing = new Size(8, 8);
            });

            Eto.Style.Add<StackLayout>("vertListStack", stack => {
                stack.Orientation = Orientation.Vertical;
                stack.Padding = new Padding(8);
                stack.Spacing = 4;
            });

            Eto.Style.Add<StackLayout>("horzListStack", stack => {
                stack.Orientation = Orientation.Horizontal;
                stack.Padding = new Padding(8);
                stack.Spacing = 4;
            });

            Eto.Style.Add<StackLayout>("dlgFooter", stack => {
                stack.Orientation = Orientation.Horizontal;
                stack.Padding = new Padding(0);
                stack.Spacing = 8;
            });

            Eto.Style.Add<StackLayout>("labtexStack", stack => {
                stack.Orientation = Orientation.Vertical;
                stack.Padding = new Padding(0);
                stack.Spacing = 2;
            });

            Eto.Style.Add<Button>("funcBtn", button => {
                button.ImagePosition = ButtonImagePosition.Left;
                button.Size = new Size(160, 26);
            });

            Eto.Style.Add<Button>("dlgBtn", button => {
                button.ImagePosition = ButtonImagePosition.Left;
                button.Size = new Size(120, 26);
            });

            Eto.Style.Add<Button>("iconBtn", button => {
                button.ImagePosition = ButtonImagePosition.Overlay;
                button.Size = new Size(26, 26);
            });
        }

        private static void InitPlatformStyles()
        {
#if OS_LINUX
            // FIXME: don't work
            Eto.Style.Add<Eto.GtkSharp.Forms.ToolBar.ToolBarHandler>("tbsi", h => {
                // executed but no result
                h.Control.ToolbarStyle = Gtk.ToolbarStyle.BothHoriz;
                //h.Control.ToolbarStyle = Gtk.ToolbarStyle.Icons;
                h.Control.IconSize = Gtk.IconSize.SmallToolbar;
            });

            Eto.Style.Add<Eto.GtkSharp.Forms.Controls.GridColumnHandler>(null, h => {
                Pango.FontDescription tpf = new Pango.FontDescription();
                tpf.Weight = Pango.Weight.Normal;
                h.Control.Button.ModifyFont(tpf);
                h.Control.Button.ModifyFg(Gtk.StateType.Normal, new Gdk.Color(0, 0, 0));
            });
#endif

#if OS_MSWIN
            Eto.Wpf.Forms.ToolBar.ToolItemHandler.DefaultImageSize = new Size(20, 20);
#endif
        }

        private void OnApplicationExit(object sender, System.ComponentModel.CancelEventArgs e)
        {
            ApplicationExit();
        }

        public override async Task Init(string[] args, bool isMDI)
        {
            await base.Init(args, isMDI);
            Application.Instance.Terminating += OnApplicationExit;
        }

        public override void Activate()
        {
            var forms = Application.Instance.Windows.ToList();
            if (forms.Count > 0) {
                forms[forms.Count - 1].Focus();
            }
        }

        public override IForm GetActiveForm()
        {
            // Form, Dialog -> Window
            foreach (var wnd in Application.Instance.Windows) {
                if (wnd.HasFocus) {
                    return wnd as IForm;
                }
            }
            return null;
        }

        public override IWindow GetActiveWindow()
        {
            IWindow activeWin = GetActiveForm() as IWindow;

            if (activeWin == null) {
                activeWin = fActiveBase;
            }

            return activeWin;
        }

        public override IntPtr GetTopWindowHandle()
        {
            return IntPtr.Zero;
        }

        public override async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            var efModal = dialog as CommonDialog;
            if (efModal == null) return false;

            //Window activeWin = GetActiveForm() as Window;
            /*if (keepModeless) {
#if OS_MSWIN
                //NativeMethods.PostMessage(mainHandle, NativeMethods.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
#endif
            }*/

            efModal.ShowModal(owner as Control);
            return await efModal.DialogResultTask;
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            Form frm = form as Form;

            if (frm != null) {
#if OS_MSWIN
                //NativeMethods.EnableWindow(frm.Handle, value);
#endif
            }
        }

        public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
            var frm = baseWin as Form;
            mf.WinRect = UIHelper.GetFormRect(frm);
            mf.WinState = gkWindowStates[(int)frm.WindowState];
        }

        private static readonly Eto.Forms.WindowState[] efWindowStates = new Eto.Forms.WindowState[] {
            Eto.Forms.WindowState.Normal,
            Eto.Forms.WindowState.Minimized,
            Eto.Forms.WindowState.Maximized
        };

        private static readonly GKCore.Options.WindowState[] gkWindowStates = new GKCore.Options.WindowState[] {
            GKCore.Options.WindowState.Normal,
            GKCore.Options.WindowState.Maximized,
            GKCore.Options.WindowState.Minimized
        };

        public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
            var frm = baseWin as Form;
            UIHelper.RestoreFormRect(frm, mf.WinRect, efWindowStates[(int)mf.WinState]);
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            var result = new EUITimer(msInterval, elapsedHandler);
            return result;
        }

        public override void Quit()
        {
            Application.Instance.Quit();
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            var activeWnd = GetActiveWindow();

            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                try {
                    workerThread.Start(progressForm);

                    ((Dialog)progressForm).ShowModal(activeWnd as Control);
                } catch (Exception ex) {
                    Logger.WriteError("ExecuteWork()", ex);
                }
            }
        }

        public override bool ExecuteWorkExt(ProgressStart proc, string title)
        {
            return false;
        }

        public override void CloseDependentWindows(IWindow owner)
        {
            var wndArr = Application.Instance.Windows.ToArray();
            foreach (var wnd in wndArr) {
                if (wnd is IWindowDependent && ((IWindowDependent)wnd).OwnerWindow == owner) {
                    wnd.Close();
                }
            }
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            var activeForm = GetActiveWindow() as Form;
            var screen = Screen.FromRectangle(activeForm.Bounds);
            return UIHelper.Rt2Rt(new Rectangle(screen.WorkingArea));
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
                form.Location = new Point(loc.X, loc.Y);
            }
        }

        public override string SelectFolder(string folderPath)
        {
            using (var fldDlg = new SelectFolderDialog()) {
                fldDlg.Directory = folderPath;
                return (fldDlg.ShowDialog(null) != DialogResult.Ok) ? string.Empty : fldDlg.Directory;
            }
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            bool result = false;

            switch (feature) {
                case Feature.GridCellFormat:
#if OS_MSWIN
                    result = false;
#else
                    result = true;
#endif
                    break;

                case Feature.InternetProxy:
                    // Not used yet, obsolete
                    result = false;
                    break;

                case Feature.MediaPlayer:
#if DIS_VLC
                    result = false;
#else
                    result = true;
#endif
                    break;

                case Feature.RecentFilesLoad:
                    // In the SDI interface, it is not clear how to implement it correctly
                    result = true;
                    break;

                case Feature.Themes:
                    result = true;
                    break;

                case Feature.OverwritePrompt:
                    result = false;
                    break;
            }

            return result;
        }

        public override void LayoutWindows(WinLayout layout)
        {
            var activeForm = GetActiveWindow() as Form;
            var screen = Screen.FromRectangle(activeForm.Bounds);
            var scrLoc = new Point(screen.WorkingArea.Location);
            var scrSize = new Size(screen.WorkingArea.Size);

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
            // InputLanguage only exists in WinForms
            return CultureInfo.CurrentUICulture.KeyboardLayoutId;
        }

        public override void SetKeyLayout(int layout)
        {
            try {
                CultureInfo.DefaultThreadCurrentUICulture = new CultureInfo(layout);
            } catch (Exception ex) {
                Logger.WriteError("EtoAppHost.SetKeyLayout()", ex);
            }
        }

        public override void SetClipboardText(string text)
        {
            using (var clipboard = new Clipboard()) {
                clipboard.Text = text;
            }
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

            var appHost = new EtoAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, EtoStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, EtoGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);
            container.Register<IThemeManager, EtoThemeManager>(LifeCycle.Singleton);

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
            ControlsManager.RegisterHandlerType(typeof(DropDown), typeof(DropDownHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(MaskedTextBox), typeof(MaskedTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericStepper), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TextBox), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(PasswordBox), typeof(PasswordBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            ControlsManager.RegisterHandlerType(typeof(ButtonMenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioMenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckMenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(TextArea), typeof(TextAreaHandler));
            ControlsManager.RegisterHandlerType(typeof(RichTextArea), typeof(RichTextAreaHandler));
            ControlsManager.RegisterHandlerType(typeof(TabPage), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(GroupBox), typeof(GroupBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ButtonToolItem), typeof(ButtonToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDropDownToolItem), typeof(ButtonToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(GKButtonToolItem), typeof(ButtonToolItemHandler));
            ControlsManager.RegisterHandlerType(typeof(Splitter), typeof(SplitterHandler));

            ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateBox), typeof(DateBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateControl), typeof(DateControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKListView), typeof(ListViewHandler));
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
