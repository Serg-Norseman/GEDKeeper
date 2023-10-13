/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using System.Threading;
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.IoC;
using GKCore.Types;
using GKUI.Components;
using GKUI.Forms;
using Xam.Plugin.TabView;
using Xamarin.Essentials;
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

        public override void Activate()
        {
        }

        public static MainPage GetMainPage()
        {
            return ((NavigationPage)Application.Current.MainPage).RootPage as MainPage;
        }

        private Page GetCurrentPage()
        {
            var navPage = (NavigationPage)Application.Current.MainPage;
            return (navPage == null) ? null : navPage.CurrentPage;
        }

        public override IForm GetActiveForm()
        {
            var form = GetCurrentPage() as IForm;
            return form;
        }

        public override IWindow GetActiveWindow()
        {
            var window = GetCurrentPage() as IWindow;
            return window;
        }

        public override IntPtr GetTopWindowHandle()
        {
            throw new NotImplementedException();
        }

        public override bool ShowModalX(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            return base.ShowModalX(dialog, owner, keepModeless);
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            throw new NotImplementedException();
        }

        public override ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            var result = new XFUITimer(msInterval, elapsedHandler);
            return result;
        }

        public override void Quit()
        {
            fPlatformSpecifics.CloseApplication();
        }

        private IProgressController GetProgressController()
        {
            var progressCtl = GetCurrentPage() as IProgressController;
            return (progressCtl == null) ? new ProgressControllerStub() : progressCtl;

            //return Container.Resolve<IProgressController>();
        }

        public override void ExecuteWork(ProgressStart proc)
        {
            try {
                var progressForm = GetProgressController();

                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                workerThread.Start(progressForm);
                workerThread.Join();
            } catch (Exception ex) {
                Logger.WriteError("ExecuteWork()", ex);
            }
        }

        public override bool ExecuteWorkExt(ProgressStart proc, string title)
        {
            return false;
        }

        public override void CloseDependentWindows(IWindow owner)
        {
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            throw new NotImplementedException();
        }

        public override string SelectFolder(string folderPath)
        {
            return string.Empty;
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            switch (feature) {
                case Feature.GridCellFormat:
                    return true; // [Q?]

                case Feature.InternetProxy:
                    return false; // [Accepted]

                case Feature.MediaPlayer:
                    return false; // [Accepted]

                case Feature.RecentFilesLoad:
                    return false; // [Q?]

                case Feature.Themes:
                    return false; // [Accepted]

                case Feature.OverwritePrompt:
                    return false; // [Q?]

                case Feature.EmbeddedLocales:
                case Feature.Mobile:
                    return true; // [Accepted]

                default:
                    return false;
            }
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
            Clipboard.SetTextAsync(text);
        }

        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for XamarinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap()
        {
            // support for legacy encodings
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

            var appHost = new XFAppHost();
            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            container.Register<IStdDialogs, XFStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, XFGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);

            // dialogs
            //container.Register<IAboutDlg, AboutDlg>(LifeCycle.Transient);
            //container.Register<IAddressEditDlg, AddressEditDlg>(LifeCycle.Transient);
            //container.Register<IAssociationEditDlg, AssociationEditDlg>(LifeCycle.Transient);
            container.Register<IBaseWindow, BaseWinSDI>(LifeCycle.Transient);
            container.Register<ICircleChartWin, CircleChartWin>(LifeCycle.Transient);
            //container.Register<ICommunicationEditDlg, CommunicationEditDlg>(LifeCycle.Transient);
            //container.Register<ICommonFilterDlg, CommonFilterDlg>(LifeCycle.Transient);
            //container.Register<IDayTipsDlg, DayTipsDlg>(LifeCycle.Transient);
            //container.Register<IEventEditDlg, EventEditDlg>(LifeCycle.Transient);
            //container.Register<IFamilyEditDlg, FamilyEditDlg>(LifeCycle.Transient);
            //container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            //container.Register<IFragmentSearchDlg, TTFamilyGroupsDlg>(LifeCycle.Transient);
            //container.Register<IGroupEditDlg, GroupEditDlg>(LifeCycle.Transient);
            //container.Register<ILanguageEditDlg, LanguageEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageSelectDlg, LanguageSelectDlg>(LifeCycle.Transient);
            //container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            //container.Register<IMapsViewerWin, MapsViewerWin>(LifeCycle.Transient);
            //container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            //container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            //container.Register<INoteEditDlgEx, NoteEditDlgEx>(LifeCycle.Transient);
            //container.Register<IOptionsDlg, OptionsDlg>(LifeCycle.Transient);
            container.Register<IOrganizerWin, OrganizerWin>(LifeCycle.Transient);
            //container.Register<IParentsEditDlg, ParentsEditDlg>(LifeCycle.Transient);
            //container.Register<IPatriarchsSearchDlg, TTPatSearchDlg>(LifeCycle.Transient);
            container.Register<IPatriarchsViewer, PatriarchsViewerWin>(LifeCycle.Transient);
            //container.Register<IPersonsFilterDlg, PersonsFilterDlg>(LifeCycle.Transient);
            //container.Register<IPlacesManagerDlg, TTPlacesManagerDlg>(LifeCycle.Transient);
            //container.Register<IPersonalNameEditDlg, PersonalNameEditDlg>(LifeCycle.Transient);
            //container.Register<IPersonEditDlg, PersonEditDlg>(LifeCycle.Transient);
            //container.Register<IPortraitSelectDlg, PortraitSelectDlg>(LifeCycle.Transient);
            //container.Register<IRecMergeDlg, TTRecMergeDlg>(LifeCycle.Transient);
            //container.Register<IRecordSelectDialog, RecordSelectDlg>(LifeCycle.Transient);
            //container.Register<IRelationshipCalculatorDlg, RelationshipCalculatorDlg>(LifeCycle.Transient);
            container.Register<IRepositoryEditDlg, RepositoryEditDlg>(LifeCycle.Transient);
            //container.Register<IResearchEditDlg, ResearchEditDlg>(LifeCycle.Transient);
            //container.Register<ISexCheckDlg, SexCheckDlg>(LifeCycle.Transient);
            //container.Register<ISourceCitEditDlg, SourceCitEditDlg>(LifeCycle.Transient);
            container.Register<ISourceEditDlg, SourceEditDlg>(LifeCycle.Transient);
            //container.Register<IScriptEditWin, ScriptEditWin>(LifeCycle.Transient);
            //container.Register<ISlideshowWin, SlideshowWin>(LifeCycle.Transient);
            container.Register<IStatisticsWin, StatisticsWin>(LifeCycle.Transient);
            //container.Register<ITaskEditDlg, TaskEditDlg>(LifeCycle.Transient);
            container.Register<ITreeChartWin, TreeChartWin>(LifeCycle.Transient);
            //container.Register<ITreeCheckDlg, TTTreeCheckDlg>(LifeCycle.Transient);
            //container.Register<ITreeCompareDlg, TTTreeCompareDlg>(LifeCycle.Transient);
            //container.Register<ITreeFilterDlg, TreeFilterDlg>(LifeCycle.Transient);
            //container.Register<ITreeMergeDlg, TTTreeMergeDlg>(LifeCycle.Transient);
            //container.Register<ITreeSplitDlg, TTTreeSplitDlg>(LifeCycle.Transient);
            container.Register<IUserRefEditDlg, UserRefEditDlg>(LifeCycle.Transient);
            //container.Register<IRecordInfoDlg, RecordInfoDlg>(LifeCycle.Transient);
            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(Switch), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Picker), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            //ControlsManager.RegisterHandlerType(typeof(MaskedEntry), typeof(MaskedTextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(XFRadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TabViewControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TabItem), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(Entry), typeof(TextBoxHandler));
            //ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            //ControlsManager.RegisterHandlerType(typeof(ButtonMenuItem), typeof(MenuItemHandler));

            ControlsManager.RegisterHandlerType(typeof(Editor), typeof(TextAreaHandler));
            //ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
        }

        #endregion

        private static IPlatformSpecifics fPlatformSpecifics;

        public static void Startup(IPlatformSpecifics platformSpecifics, string[] args)
        {
            fPlatformSpecifics = platformSpecifics;

            ConfigureBootstrap();
            //CheckPortable(args);
            Logger.Init(GetLogFilename());
            LogSysInfo();
        }
    }
}
