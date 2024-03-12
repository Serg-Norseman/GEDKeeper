/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Options;
using NSubstitute;

namespace GKTests.Stubs
{
    public sealed class AppHostStub : AppHost
    {
        static AppHostStub()
        {
            SetAppSign("GEDKeeperTest");
        }

        public AppHostStub()
        {
        }

        public override IWindow GetActiveWindow()
        {
            return null;
        }

        public override IntPtr GetTopWindowHandle()
        {
            return IntPtr.Zero;
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
        }

        public override void SaveWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        public override void RestoreWinState(IBaseWindow baseWin, MRUFile mf)
        {
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            return Substitute.For<ITimer>();
        }

        public override void Quit()
        {
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            IView activeWnd = GetActiveWindow();

            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                try {
                    workerThread.Start(progressForm);

                    //progressForm.ShowModalX(activeWnd);
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
            return ExtRect.Empty;
        }

        public override void WidgetLocate(IWidgetForm view, WidgetLocation location)
        {
        }

        public override string SelectFolder(string folderPath)
        {
            return string.Empty;
        }

        public override int GetKeyLayout()
        {
            return 0;
        }

        public override void SetKeyLayout(int layout)
        {
        }

        public override void SetClipboardText(string text)
        {
        }

        public override void Activate()
        {
        }

        public override IForm GetActiveForm()
        {
            return null;
        }

        public override void CloseDependentWindows(IWindow owner)
        {
        }

        public static void ConfigureBootstrap()
        {
            var appHost = new AppHostStub();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, StdDialogsStub>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, GfxProviderStub>(LifeCycle.Singleton);
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

            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);*/

            //ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            //ControlsManager.RegisterHandlerType(typeof(Switch), typeof(CheckBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(XFComboBox), typeof(ComboBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(ComboBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            //ControlsManager.RegisterHandlerType(typeof(MaskedEntry), typeof(MaskedTextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            //ControlsManager.RegisterHandlerType(typeof(XFRadioButton), typeof(RadioButtonHandler));
            //ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler));
            //ControlsManager.RegisterHandlerType(typeof(Entry), typeof(TextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            //ControlsManager.RegisterHandlerType(typeof(ButtonMenuItem), typeof(MenuItemHandler));

            //ControlsManager.RegisterHandlerType(typeof(TextArea), typeof(TextAreaHandler));
            //ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
        }

        public static void Startup(string[] args)
        {
            ConfigureBootstrap();
            CheckPortable(args);
            Logger.Init(GetLogFilename());
            LogSysInfo();

            //AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;
        }

        private static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs e)
        {
            // Saving the copy for restoration
            AppHost.Instance.CriticalSave();
            Logger.WriteError("GK.UnhandledExceptionsHandler()", (Exception)e.ExceptionObject);
        }

        public override void LayoutWindows(WinLayout layout)
        {
        }

        public override void SetWindowBounds(IWindow window, ExtRect bounds)
        {
        }
    }
}
