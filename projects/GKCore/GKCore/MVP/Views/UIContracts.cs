/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.MVP.Views
{
    public interface IAboutDlg : ICommonDialog, IView
    {
    }


    public interface IAddressEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMAddress Address { get; set; }

        ITextBox Country { get; }
        ITextBox State { get; }
        ITextBox City { get; }
        ITextBox PostalCode { get; }
        ITextBox AddressLine { get; }

        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IAssociationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMAssociation Association { get; set; }

        ITextBox Person { get; }
        IComboBox Relation { get; }
    }


    public interface IBaseWindowView : IBaseWindow, IView
    {
        ITabControl RecordTabs { get; }
        IMenuItem ReportsItem { get; }
        IMenuItem PluginsItem { get; }

        bool CheckModified();
    }


    public interface ICircleChartWin : IChartWindow, IView
    {
        ICircleChart CircleChart { get; }
    }


    public interface ICommonFilterDlg : ICommonDialog, IView
    {
        IFilterGridView FilterGrid { get; }
    }


    public interface ICommunicationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMCommunicationRecord Communication { get; set; }

        ITextBox Corresponder { get; }
        IComboBox CorrType { get; }
        ITextBox Date { get; }
        IComboBox Dir { get; }
        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
    }


    public interface IDayTipsDlg : ICommonDialog, IView
    {
        bool ShowTipsChecked { get; set; }

        void Init(string caption, bool showTipsChecked, StringList tips);

        ILabel TitleLabel { get; }
        ITextBox TipText { get; }
        IButton NextButton { get; }
    }


    public interface IEventEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMCustomEvent Event { get; set; }

        IComboBox EventType { get; }
        IComboBox EventDateType { get; }

        ICheckBox Date1BC { get; }
        ICheckBox Date2BC { get; }

        IComboBox Date1Calendar { get; }
        IComboBox Date2Calendar { get; }

        IDateBoxHandler Date1 { get; }
        IDateBoxHandler Date2 { get; }

        IComboBox Attribute { get; }
        ITextBox Place { get; }
        ITextBox EventName { get; }
        ITextBox Cause { get; }
        ITextBox Agency { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }

        void SetLocationMode(bool active);
    }


    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMFamilyRecord Family { get; set; }

        void SetTarget(TargetMode targetType, GDMIndividualRecord target);
        void LockEditor(bool locked);
        void SetHusband(string value);
        void SetWife(string value);

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ChildrenList { get; }
        ISheetList EventsList { get; }

        IComboBox MarriageStatus { get; }
        IComboBox Restriction { get; }
        ITextBox Husband { get; }
        ITextBox Wife { get; }
    }


    public interface IFilePropertiesDlg : ICommonDialog, IBaseEditor, IView
    {
        IListView RecordStats { get; }

        ITextBox Language { get; }
        ITextBox Name { get; }
        ITextBox Address { get; }
        ITextBox Tel { get; }
    }


    public interface IGroupEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMGroupRecord Group { get; set; }

        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList MembersList { get; }
    }


    public interface ILanguageEditDlg : ICommonDialog, IView
    {
        GDMLanguageID LanguageID { get; set; }

        IComboBox LanguageCombo { get; }
    }


    public interface ILanguageSelectDlg : ICommonDialog, IView
    {
        int SelectedLanguage { get; set; }

        IListViewEx LanguagesList { get; }
    }


    public interface ILocationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMLocationRecord LocationRecord { get; set; }

        IMapBrowser MapBrowser { get; }
        ISheetList MediaList { get; }
        ISheetList NotesList { get; }
        IListView GeoCoordsList { get; }
        ITextBox Name { get; }
        ITextBox Latitude { get; }
        ITextBox Longitude { get; }
    }


    public interface IMapsViewerWin : IWindow, IView
    {
        IMapBrowser MapBrowser { get; }
        IComboBox PersonsCombo { get; }
        ITreeView PlacesTree { get; }
        IButton SelectPlacesBtn { get; }
        ICheckBox BirthCheck { get; }
        ICheckBox DeathCheck { get; }
        ICheckBox ResidenceCheck { get; }
        ICheckBox LinesVisibleCheck { get; }
        IRadioButton TotalRadio { get; }
        IRadioButton SelectedRadio { get; }

        ITVNode FindTreeNode(string place);
    }


    public interface IMediaEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMMultimediaRecord MediaRec { get; set; }

        ISheetList NotesList { get; }
        ISheetList SourcesList { get; }

        IComboBox MediaType { get; }
        IComboBox StoreType { get; }
        ITextBox Name { get; }
        ITextBox File { get; }
        IButton FileSelectButton { get; }
    }


    public interface IMediaViewerWin : IWindow, IView
    {
        GDMFileReferenceWithTitle FileRef { get; set; }

        void SetViewImage(IImage img, GDMFileReferenceWithTitle fileRef);
        void SetViewMedia(string mediaFile);
        void SetViewText(string text);
        void SetViewRTF(string text);
        void SetViewHTML(Stream stm);
        void DisposeViewControl();
    }


    public interface INameEditDlg : ICommonDialog, IView
    {
        NameEntry IName { get; set; }

        ITextBox Name { get; }
        ITextBox FPatr { get; }
        ITextBox MPatr { get; }
        IComboBox SexCombo { get; }
    }


    public interface INoteEdit : ICommonDialog, IBaseEditor, IView
    {
        GDMNoteRecord NoteRecord { get; set; }

        ITextBox Note { get; }
    }


    public interface INoteEditDlg : INoteEdit
    {
    }

    public interface INoteEditDlgEx : INoteEdit
    {
    }


    public interface IOrganizerWin : ICommonDialog, IView
    {
        ISheetList AdrList { get; }
        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IOptionsDlg : ICommonDialog, IView
    {
        void SetPage(OptionsPage page);
    }


    public interface IParentsEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMChildToFamilyLink Link { get; set; }
        GDMIndividualRecord Person { get; set; }

        ITextBox Father { get; }
        ITextBox Mother { get; }
        ITextBox ChildName { get; }
        IComboBox LinkageTypeCombo { get; }

        void SetParentsAvl(bool avail);
        void SetFatherAvl(bool avail);
        void SetMotherAvl(bool avail);
    }


    public interface IPersonalNameEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMPersonalName PersonalName { get; set; }

        ILabel SurnameLabel { get; }
        ITextBox Surname { get; }
        ITextBox Name { get; }
        ITextBox Patronymic { get; }
        IComboBox NameType { get; }
        ITextBox NamePrefix { get; }
        ITextBox Nickname { get; }
        ITextBox SurnamePrefix { get; }
        ITextBox NameSuffix { get; }
        ITextBox MarriedSurname { get; }
        IComboBox Language { get; }
    }


    public interface IPersonEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMIndividualRecord Person { get; set; }
        GDMIndividualRecord Target { get; set; }
        TargetMode TargetMode { get; set; }
        void SetNeedSex(GDMSex needSex);

        ISheetList EventsList { get; }
        ISheetList SpousesList { get; }
        ISheetList AssociationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList UserRefList { get; }
        ISheetList NamesList { get; }
        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ParentsList { get; }

        IPortraitControl Portrait { get; }
        ITextBox Father { get; }
        ITextBox Mother { get; }
        ITextBox Surname { get; }
        ITextBox Name { get; }
        IComboBox Patronymic { get; }
        ITextBox NamePrefix { get; }
        ITextBox Nickname { get; }
        ITextBox SurnamePrefix { get; }
        ITextBox NameSuffix { get; }
        ITextBox MarriedSurname { get; }

        ILabel SurnameLabel { get; }
        IComboBox RestrictionCombo { get; }
        IComboBox SexCombo { get; }

        ICheckBox Patriarch { get; }
        ICheckBox Bookmark { get; }

        void SetParentsAvl(bool avail, bool locked);
        void SetFatherAvl(bool avail, bool locked);
        void SetMotherAvl(bool avail, bool locked);
        //void UpdatePortrait(bool totalUpdate);

        void SetPortrait(IImage portrait);
        void SetPortraitAvl(bool avail, bool locked);
    }


    public interface IPersonsFilterDlg : ICommonDialog, IView
    {
        IComboBox SourceCombo { get; }
        IComboBox GroupCombo { get; }
        ITextBox AliveBeforeDate { get; }
        ICheckBox OnlyPatriarchsCheck { get; }
        IComboBox EventValCombo { get; }
        IComboBox ResidenceCombo { get; }
        IComboBox NameCombo { get; }

        void SetLifeRadio(int lifeSel);
        void SetSexRadio(int sexSel);
        int GetLifeRadio();
        int GetSexRadio();
        void SetLifeEnabled(bool value);
    }


    public interface IPortraitSelectDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMMultimediaLink MultimediaLink { get; set; }

        IImageView ImageCtl { get; }
    }


    public interface IQuickSearchDlg : IView, ILocalization
    {
        ITextBox SearchPattern { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor, IView
    {
        string FastFilter { get; set; }
        TargetMode TargetMode { get; set; }
        GDMIndividualRecord TargetIndividual { get; set; }
        GDMSex NeedSex { get; set; }
        GDMRecord ResultRecord { get; set; }

        IListViewEx RecordsList { get; }
    }


    public interface IRelationshipCalculatorDlg : ICommonDialog, IView
    {
        ILabel Label1 { get; }
        ILabel Label2 { get; }
        ITextBox Person1 { get; }
        ITextBox Person2 { get; }
        ITextBox Result { get; }
    }


    public interface IRepositoryEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMRepositoryRecord Repository { get; set; }

        ISheetList NotesList { get; }
        ITextBox Name { get; }
    }


    public interface IResearchEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMResearchRecord Research { get; set; }

        ISheetList TasksList { get; }
        ISheetList CommunicationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList NotesList { get; }

        ITextBox Name { get; }
        IComboBox Priority { get; }
        IComboBox Status { get; }
        ITextBox StartDate { get; }
        ITextBox StopDate { get; }
        INumericBox Percent { get; }
    }


    public interface IScriptEditWin : ICommonDialog, ILocalization, IView
    {
        ITextBox ScriptText { get; }
        ITextBox DebugOutput { get; }

        string FileName { get; set; }
        bool Modified { get; set; }

        bool CheckModified();
    }


    public interface ISexCheckDlg : ICommonDialog, IView
    {
        string IndividualName { get; set; }
        GDMSex Sex { get; set; }
    }


    public interface ISlideshowWin : IWindow, IStatusForm, IView
    {
        void SetImage(IImage image);
        void UpdateControls();
    }


    public interface ISourceCitEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMSourceCitation SourceCitation { get; set; }

        ITextBox Page { get; }
        IComboBox Certainty { get; }
        IComboBox Source { get; }
    }


    public interface ISourceEditDlg : ICommonDialog, IBaseEditor, IView<GDMSourceRecord, ISourceEditDlg>
    {
        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList RepositoriesList { get; }

        ITextBox ShortTitle { get; }
        ITextBox Author { get; }
        ITextBox Title { get; }
        ITextBox Publication { get; }
        ITextBox Text { get; }
    }


    public interface IStatisticsWin : IWindow, IView
    {
        IGraphControl Graph { get; }
        IListView ListStats { get; }
        IListView Summary { get; }
        IComboBox StatsType { get; }
    }


    public interface ITaskEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMTaskRecord Task { get; set; }

        ISheetList NotesList { get; }
        IComboBox Priority { get; }
        ITextBox StartDate { get; }
        ITextBox StopDate { get; }
        IComboBox GoalType { get; }
        ITextBox Goal { get; }
        IButton GoalSelect { get; }
    }


    public interface ITreeChartWin : IChartWindow, IView
    {
        ITreeChartBox TreeBox { get; }

        void GenChart(TreeChartKind chartKind);
    }


    public interface ITreeFilterDlg : ICommonDialog, IView
    {
        ChartFilter Filter { get; set; }

        ISheetList PersonsList { get; }
        INumericBox YearNum { get; }
        IComboBox SourceCombo { get; }

        int GetCutModeRadio();
        void SetCutModeRadio(int cutMode);
    }


    public interface IUserRefEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMUserReference UserRef { get; set; }

        IComboBox Ref { get; }
        IComboBox RefType { get; }
    }



    public interface IFragmentSearchDlg : ICommonDialog, IBaseEditor, IView
    {
        ITreeView GroupsTree { get; }
        ILogChart LogChart { get; }
    }


    public interface IPatriarchsSearchDlg : ICommonDialog, IBaseEditor, IView
    {
        INumericBox MinGensNum { get; }
        ICheckBox WithoutDatesCheck { get; }
        IListView PatriarchsList { get; }
    }


    public interface IPatriarchsViewer : IWindow, IView
    {
    }


    public interface IPlacesManagerDlg : ICommonDialog, IBaseEditor, IView
    {
        IListViewEx PlacesList { get; }
    }


    public interface IRecMergeDlg : ICommonDialog, IBaseEditor, IView
    {
        IMergeControl MergeCtl { get; }
        IButton SkipBtn { get; }
        IProgressBar ProgressBar { get; }
        ICheckBox IndistinctMatchingChk { get; }
        INumericBox NameAccuracyNum { get; }
        ICheckBox BirthYearChk { get; }
        INumericBox YearInaccuracyNum { get; }
    }


    public interface ITreeCheckDlg : ICommonDialog, IBaseEditor, IView
    {
        IListViewEx ChecksList { get; }
    }


    public enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

    public interface ITreeCompareDlg : ICommonDialog, IBaseEditor, IView
    {
        ITextBox ExternalBase { get; }
        ITextBox CompareOutput { get; }

        TreeMatchType GetTreeMatchType();
    }


    public interface ITreeMergeDlg : ICommonDialog, IBaseEditor, IView
    {
        ITextBox UpdateBase { get; }
        ITextBox SyncLog { get; }
    }


    public interface ITreeSplitDlg : ICommonDialog, IBaseEditor, IView
    {
        IListViewEx SelectedList { get; }
        IListViewEx SkippedList { get; }
    }


    public interface IRecordInfoDlg : ICommonDialog, IBaseEditor, IView
    {
        GDMRecord Record { get; set; }

        IHyperView HyperView { get; }
    }
}
