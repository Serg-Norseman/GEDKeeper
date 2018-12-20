/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Reflection;
using System.Windows.Forms;

using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKUI.Components;
using GKUI.Forms;

namespace GKUI.Providers
{
    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// WinForms.
    /// </summary>
    public sealed class WFAppHost : AppHost
    {
        private readonly ApplicationContext fAppContext;

        public ApplicationContext AppContext
        {
            get { return fAppContext; }
        }

        static WFAppHost()
        {
            SetAppSign("GEDKeeper2");
        }

        public WFAppHost() : base()
        {
            fAppContext = new ApplicationContext();
            Application.ApplicationExit += this.OnApplicationExit;
        }

        private void OnApplicationExit(object sender, EventArgs e)
        {
        }

        public override void Init(string[] args, bool isMDI)
        {
            base.Init(args, isMDI);
        }

        // FIXME
        public override IWindow GetActiveWindow()
        {
            Form activeForm = Form.ActiveForm;

            // only for tests!
            if (activeForm == null && fRunningForms.Count > 0) {
                activeForm = (Form)fRunningForms[0];
            }

            return (activeForm is IWindow) ? (IWindow)activeForm : null;
        }

        // FIXME!
        public override IntPtr GetTopWindowHandle()
        {
            IntPtr mainHandle = IntPtr.Zero;

            return mainHandle;
        }

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (fRunningForms.Count == 0) {
                fAppContext.ExitThread();
            }
        }

        public override void ShowWindow(IWindow window)
        {
            Form frm = window as Form;

            if (frm != null) {
                frm.ShowInTaskbar = true;
                frm.Show();
            }
        }

        public override bool ShowModalX(ICommonDialog form, bool keepModeless = false)
        {
            IntPtr mainHandle = GetTopWindowHandle();

            if (keepModeless) {
                foreach (IWindow win in fRunningForms) {
                    if (win is IBaseWindow) {
                        IntPtr handle = ((Form)win).Handle;

                        #if !__MonoCS__
                        NativeMethods.PostMessage(handle, NativeMethods.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
                        #endif
                    }
                }
            }

            UIHelper.CenterFormByParent((Form)form, mainHandle);

            return base.ShowModalX(form, keepModeless);
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            Form frm = form as Form;

            if (frm != null) {
                #if !__MonoCS__
                NativeMethods.EnableWindow(frm.Handle, value);
                #endif
            }
        }

        protected override void UpdateLang()
        {
            foreach (IWindow win in fRunningForms) {
                win.SetLang();
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
                    mf.WinState = (WindowState)frm.WindowState;
                }
            }
        }

        public override void RestoreWinMRU(IBaseWindow baseWin)
        {
            if (baseWin != null) {
                int idx = AppHost.Options.MRUFiles_IndexOf(baseWin.Context.FileName);
                if (idx >= 0) {
                    var frm = baseWin as Form;
                    MRUFile mf = AppHost.Options.MRUFiles[idx];
                    UIHelper.RestoreFormRect(frm, mf.WinRect, (FormWindowState)mf.WinState);
                }
            }
        }

        public override void SaveLastBases()
        {
            AppHost.Options.ClearLastBases();

            foreach (IWindow win in fRunningForms) {
                var baseWin = win as IBaseWindow;
                if (baseWin != null) {
                    AppHost.Options.AddLastBase(baseWin.Context.FileName);
                }
            }
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            var result = new WinUITimer(msInterval, elapsedHandler);
            return result;
        }

        public override void Quit()
        {
            // FIXME: Controversial issue...
            //AppHost.Instance.SaveLastBases();
            Application.Exit();
        }

        #region KeyLayout functions

        public override int GetKeyLayout()
        {
            #if __MonoCS__
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
                Logger.LogWrite("WinFormsAppHost.SetKeyLayout(): " + ex.Message);
            }
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

            var appHost = new WFAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, WFStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, WFGfxProvider>(LifeCycle.Singleton);
            container.Register<IProgressController, ProgressController>(LifeCycle.Singleton);
            container.Register<ITreeChartBox, TreeChartBox>(LifeCycle.Transient);

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

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(MaskedTextBox), typeof(MaskedTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TextBox), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolStripMenuItem), typeof(MenuItemHandler));

            ControlsManager.RegisterHandlerType(typeof(ToolStripComboBox), typeof(ToolStripComboBoxHandler));
        }

        #endregion
    }
}
