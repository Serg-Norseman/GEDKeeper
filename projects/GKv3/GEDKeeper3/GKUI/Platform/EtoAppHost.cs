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
using System.Globalization;
using System.Text;
using System.Threading;
using BSLib;
using BSLib.Design.IoC;
using BSLib.Design.MVP;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using GKUI.Components;
using GKUI.Forms;

namespace GKUI.Platform
{
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
                button.ImagePosition = ButtonImagePosition.Left;
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
        }

        private void OnApplicationExit(object sender, System.ComponentModel.CancelEventArgs e)
        {
            //AppHost.Instance.SaveLastBases();
        }

        public override void Init(string[] args, bool isMDI)
        {
            base.Init(args, isMDI);
            Application.Instance.Terminating += OnApplicationExit;
        }

        public override IWindow GetActiveWindow()
        {
            IWindow activeWin = fActiveBase;

            if (activeWin == null) {
                foreach (var wnd in Application.Instance.Windows) {
                    if (wnd.HasFocus) {
                        activeWin = wnd as IWindow;
                        break;
                    }
                }
            }

            return activeWin;
        }

        public override IntPtr GetTopWindowHandle()
        {
            return IntPtr.Zero;
        }

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (fRunningForms.Count == 0) {
                Application.Instance.Quit();
            }
        }

        public override bool ShowModalX(ICommonDialog form, bool keepModeless = false)
        {
            Window activeWin = GetActiveWindow() as Window;

            if (keepModeless) {
#if !MONO
                //NativeMethods.PostMessage(mainHandle, NativeMethods.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
#endif
            }

            //UIHelper.CenterFormByParent((Window)form, mainHandle);

            return (form != null && form.ShowModalX(activeWin));
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            Form frm = form as Form;

            if (frm != null) {
#if !MONO
                //NativeMethods.EnableWindow(frm.Handle, value);
#endif
            }
        }

        protected override void UpdateLang()
        {
            foreach (IWindow win in fRunningForms) {
                win.SetLocale();
            }
        }

        public override void ApplyOptions()
        {
            base.ApplyOptions();

            foreach (IWindow win in fRunningForms) {
                if (win is IWorkWindow) {
                    (win as IWorkWindow).UpdateSettings();
                }
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            base.BaseClosed(baseWin);

            SaveWinMRU(baseWin);
        }

        protected override void UpdateMRU()
        {
            foreach (IWindow win in fRunningForms) {
                if (win is IBaseWindow) {
                    (win as BaseWinSDI).UpdateMRU();
                }
            }
        }

        public override void SaveWinMRU(IBaseWindow baseWin)
        {
            if (baseWin != null) {
                int idx = AppHost.Options.MRUFiles_IndexOf(baseWin.Context.FileName);
                if (idx >= 0) {
                    var frm = baseWin as Form;
                    MRUFile mf = AppHost.Options.MRUFiles[idx];
                    mf.WinRect = UIHelper.GetFormRect(frm);
                    mf.WinState = gkWindowStates[(int)frm.WindowState];
                }
            }
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

        public override void RestoreWinMRU(IBaseWindow baseWin)
        {
            if (baseWin != null) {
                int idx = AppHost.Options.MRUFiles_IndexOf(baseWin.Context.FileName);
                if (idx >= 0) {
                    var frm = baseWin as Form;
                    MRUFile mf = AppHost.Options.MRUFiles[idx];
                    UIHelper.RestoreFormRect(frm, mf.WinRect, efWindowStates[(int)mf.WinState]);
                }
            }
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
            var activeWnd = GetActiveWindow() as Window;

            using (var progressForm = ResolveDialog<IProgressController>()) {
                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                try {
                    workerThread.Start(progressForm);

                    progressForm.ShowModalX(activeWnd);
                } catch (Exception ex) {
                    Logger.WriteError("ExecuteWork()", ex);
                }
            }
        }

        public override bool ExecuteWorkExt(ProgressStart proc, string title)
        {
            return false;
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            var activeForm = GetActiveWindow() as Form;
            var screen = Screen.FromRectangle(activeForm.Bounds);
            return UIHelper.Rt2Rt(new Rectangle(screen.WorkingArea));
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
            UIHelper.SetClipboardText(text);
        }

        #endregion

        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for WinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap(bool mdi)
        {
            if (mdi)
                throw new ArgumentException("MDI obsolete");

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
            container.Register<IGraphicsProviderEx, EtoGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);

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
            container.Register<IMapsViewerWin, MapsViewerWin>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
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

            container.Register<IProgressController, ProgressDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(MaskedTextBox), typeof(MaskedTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericStepper), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButtonEx), typeof(RadioButtonHandler));
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
            ControlsManager.RegisterHandlerType(typeof(DropDownToolItem), typeof(ButtonToolItemHandler));

            ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateBox), typeof(DateBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateControl), typeof(DateControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKListView), typeof(ListViewHandler));
        }

        #endregion

        public static void Startup(string[] args)
        {
            ConfigureBootstrap(false);
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
