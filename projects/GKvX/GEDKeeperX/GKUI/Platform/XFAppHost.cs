﻿/*
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
using System.Threading;
using BSLib;
using BSLib.Design.IoC;
using BSLib.Design.MVP;
using GKCore;
using GKCore.Interfaces;
using Xamarin.Forms;
using XFRadioButton = Xamarin.Forms.RadioButton;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// XamarinForms.
    /// </summary>
    public sealed class XFAppHost : AppHost
    {
        static XFAppHost()
        {
            SetAppSign("GEDKeeperX");
        }

        public XFAppHost()
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
        }

        public override bool ShowModalX(ICommonDialog form, bool keepModeless = false)
        {
            return base.ShowModalX(form, keepModeless);
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            throw new NotImplementedException();
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
            throw new NotImplementedException();
        }

        public override void SaveWinMRU(IBaseWindow baseWin)
        {
            throw new NotImplementedException();
        }

        public override void RestoreWinMRU(IBaseWindow baseWin)
        {
            throw new NotImplementedException();
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            throw new NotImplementedException();
        }

        public override void Quit()
        {
            Application.Current.Quit();
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            var activeWnd = GetActiveWindow() as ContentPage;

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
            throw new NotImplementedException();
        }

        #region KeyLayout functions

        public override int GetKeyLayout()
        {
            throw new NotImplementedException();
        }

        public override void SetKeyLayout(int layout)
        {
            throw new NotImplementedException();
        }

        public override void SetClipboardText(string text)
        {
            throw new NotImplementedException();
        }

        #endregion

        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for XamarinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap(bool mdi)
        {
            var appHost = new XFAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            /*container.Register<IStdDialogs, EtoStdDialogs>(LifeCycle.Singleton);*/
            container.Register<IGraphicsProviderEx, XFGfxProvider>(LifeCycle.Singleton);
            /*container.Register<ITreeChartBox, TreeChartBox>(LifeCycle.Transient);

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

            container.Register<IProgressController, ProgressDlg>(LifeCycle.Transient);*/

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(Switch), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(XFComboBox), typeof(ComboBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            //ControlsManager.RegisterHandlerType(typeof(MaskedEntry), typeof(MaskedTextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(XFRadioButton), typeof(RadioButtonHandler));
            //ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(Entry), typeof(TextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            //ControlsManager.RegisterHandlerType(typeof(ButtonMenuItem), typeof(MenuItemHandler));

            //ControlsManager.RegisterHandlerType(typeof(TextArea), typeof(TextAreaHandler));
            //ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
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
