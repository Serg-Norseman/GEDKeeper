/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Utilities;
using GKCore.Validation;
using GKUI.Components;
using GKUI.Forms;
using GKUI.Platform.Handlers;
using Xam.Plugin.TabView;
using Xamarin.Essentials;
using Xamarin.Forms;
using XFIKCheckBox = Plugin.InputKit.Shared.Controls.CheckBox;
using XFIKRadioButton = Plugin.InputKit.Shared.Controls.RadioButton;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// XamarinForms.
    /// </summary>
    public sealed class XFAppHost : AppHost
    {
        private IBaseWindow fCurrentBase;

        static XFAppHost()
        {
            SetAppSign("GEDKeeperX");
        }

        public XFAppHost()
        {
        }

        public override async Task<IBaseWindow> CreateBase(string fileName)
        {
            // FIXME: temp solution
            IBaseWindow result = await base.CreateBase(fileName);
            fCurrentBase = result;
            return result;
        }

        public override IBaseWindow GetCurrentFile(bool extMode = false)
        {
            // FIXME: temp solution
            return fCurrentBase;
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

        public override async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            var xfModal = dialog as CommonDialog;
            if (xfModal == null) return false;

            await GetMainPage().Navigation.PushModalAsync(xfModal);
            return await xfModal.DialogResultTask;
        }

        public override async void OpenURL(string uriString)
        {
            await Launcher.TryOpenAsync(new Uri(uriString));
        }

        public override IEnumerable<T> GetRunningForms<T>()
        {
            var navPage = (NavigationPage)Application.Current.MainPage;
            foreach (var page in navPage.Pages) {
                T form = page as T;
                if (form != null) {
                    yield return form;
                }
            }
        }

        protected override void UpdateLang()
        {
            var navPage = (NavigationPage)Application.Current.MainPage;

            var mainPage = (MainPage)navPage.RootPage;
            mainPage.SetLocale();

            var menuPage = (MenuPage)mainPage.Master;
            menuPage.SetLocale();

            var launchPage = (LaunchPage)mainPage.Detail;
            launchPage.SetLocale();

            var baseWin = GetCurrentFile();
            baseWin.SetLocale();

            base.UpdateLang();
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

        public override bool ExecuteWork(ProgressStart proc, string title = "")
        {
            try {
                var progressForm = GetProgressController();

                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                workerThread.Start(progressForm);
                workerThread.Join();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("ExecuteWork()", ex);
                return false;
            }
        }

        public override string GetExternalStorageDirectory()
        {
            return fPlatformSpecifics.GetExternalStorageDirectory();
        }

        public override ExtRect GetActiveScreenWorkingArea()
        {
            throw new NotImplementedException();
        }

        public override void SetWindowBounds(IWindow window, ExtRect bounds)
        {
            // not for mobile
        }

        public override string SelectFolder(string folderPath)
        {
            return string.Empty;
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            bool result = false;

            switch (feature) {
                case Feature.GridCellFormat:
                    result = true; // [Q?]
                    break;

                case Feature.InternetProxy:
                    result = false; // [Accepted]
                    break;

                case Feature.MediaPlayer:
                    result = false; // [Accepted]
                    break;

                case Feature.RecentFilesLoad:
                    result = false; // [Q?]
                    break;

                case Feature.Themes:
                    result = false; // [Accepted]
                    break;

                case Feature.OverwritePrompt:
                    result = false; // [Q?]
                    break;

                case Feature.EmbeddedLocales:
                case Feature.Mobile:
                    result = true; // [Accepted]
                    break;

                case Feature.Graphics:
                    result = true;
                    break;

                case Feature.DesktopV3:
                    result = false;
                    break;
            }

            return result;
        }

        public override void SetClipboardText(string text)
        {
            Clipboard.SetTextAsync(text);
        }

        public override void SetClipboardImage(object image)
        {
            // not supported
        }

        public override void Invoke(Action action)
        {
            action();
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
            var container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();
            ValidationFactory.InitGDMValidators();

            // controls and other
            container.Register<IStdDialogs, XFStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, XFGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);

            // dialogs
            container.Register<IAboutDlg, AboutPage>(LifeCycle.Transient);
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
            container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            container.Register<ILocationNameEditDlg, LocationNameEditDlg>(LifeCycle.Transient);
            container.Register<ILocationLinkEditDlg, LocationLinkEditDlg>(LifeCycle.Transient);
            container.Register<IMapsViewerWin, MapsViewerWin>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            container.Register<IMediaViewerWin, MediaViewerWin>(LifeCycle.Transient);
            container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
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
            container.Register<ISourceCallNumberEditDlg, SourceCallNumberEditDlg>(LifeCycle.Transient);
            container.Register<IRepositoryCitEditDlg, RepositoryCitEditDlg>(LifeCycle.Transient);

            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(XFIKCheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Editor), typeof(TextAreaHandler));
            ControlsManager.RegisterHandlerType(typeof(Entry), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericStepper), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(XFIKRadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TabItem), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(TabViewControl), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKTreeView), typeof(TreeViewHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuItem), typeof(MenuItemHandler));

            ControlsManager.RegisterHandlerType(typeof(GroupBox), typeof(GroupBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ToolbarItem), typeof(ToolbarItemHandler));

            ControlsManager.RegisterHandlerType(typeof(GKComboBox), typeof(PickerHandler));
            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateBox), typeof(DateBoxHandler));
        }

        #endregion

        private static IPlatformSpecifics fPlatformSpecifics;

        public static void Startup(IPlatformSpecifics platformSpecifics)
        {
            fPlatformSpecifics = platformSpecifics;

            ConfigureBootstrap();
            Logger.Init(GetLogFilename());
            LogSysInfo();
        }
    }
}
