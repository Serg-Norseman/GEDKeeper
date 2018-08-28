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

using BSLib;
using GKCommon.GEDCOM;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Maps;
using GKCore.Types;

namespace GKCore.UIContracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IMapBrowser
    {
        bool ShowPoints { get; set; }
        bool ShowLines { get; set; }
        ExtList<GeoPoint> MapPoints { get; }

        int AddPoint(double latitude, double longitude, string hint);
        void ClearPoints();
        void DeletePoint(int index);
        void BeginUpdate();
        void EndUpdate();
        void InitMap();
        void RefreshPoints();
        void SaveSnapshot(string fileName);
        void SetCenter(double latitude, double longitude, int scale);
        void ZoomToBounds();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IMenuItem
    {
        bool Checked { get; set; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface ITextControl
    {
        void AppendText(string text);
        void Clear();
    }


    /// <summary>
    /// The interface of the class for working with WinForms dialogs.
    /// </summary>
    public interface IStdDialogs
    {
        IFont SelectFont(IFont font);
        string GetOpenFile(string title, string context, string filter,
                           int filterIndex, string defaultExt);
        string GetSaveFile(string filter);
        string GetSaveFile(string title, string context, string filter, int filterIndex, string defaultExt,
                           string suggestedFileName, bool overwritePrompt = true);

        void ShowMessage(string msg);
        void ShowError(string msg);
        bool ShowQuestionYN(string msg);
        void ShowWarning(string msg);

        bool GetInput(string prompt, ref string value);
        bool GetPassword(string prompt, ref string value);
    }


    public interface IView
    {
    }

    public interface IAddressEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMAddress Address { get; set; }

        ITextBoxHandler Country { get; }
        ITextBoxHandler State { get; }
        ITextBoxHandler City { get; }
        ITextBoxHandler PostalCode { get; }
        ITextBoxHandler AddressLine { get; }

        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }

    public interface IAssociationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMAssociation Association { get; set; }

        ITextBoxHandler Person { get; }
        IComboBoxHandler Relation { get; }
    }

    public interface ICommunicationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMCommunicationRecord Communication { get; set; }
    }

    public interface IEventEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMCustomEvent Event { get; set; }

        IComboBoxHandler EventType { get; }
        IComboBoxHandler EventDateType { get; }

        ICheckBoxHandler Date1BC { get; }
        ICheckBoxHandler Date2BC { get; }

        IComboBoxHandler Date1Calendar { get; }
        IComboBoxHandler Date2Calendar { get; }

        ITextBoxHandler Date1 { get; }
        ITextBoxHandler Date2 { get; }

        IComboBoxHandler Attribute { get; }
        ITextBoxHandler Place { get; }
        ITextBoxHandler EventName { get; }
        ITextBoxHandler Cause { get; }
        ITextBoxHandler Agency { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }

        void SetLocationMode(bool active);
    }

    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMFamilyRecord Family { get; set; }
        void SetTarget(TargetMode targetType, GEDCOMIndividualRecord target);
    }

    public interface IGroupEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMGroupRecord Group { get; set; }
    }

    public interface ILanguageEditDlg : ICommonDialog
    {
        GEDCOMLanguageID LanguageID { get; set; }
    }

    public interface ILanguageSelectDlg : ICommonDialog
    {
        int SelectedLanguage { get; set; }
    }

    public interface ILocationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMLocationRecord LocationRecord { get; set; }
    }

    public interface IMediaEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMMultimediaRecord MediaRec { get; set; }
    }

    public interface INameEditDlg : ICommonDialog, IView
    {
        NameEntry IName { get; set; }
    }

    public interface INoteEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMNoteRecord NoteRecord { get; set; }
    }

    public interface INoteEditDlgEx : ICommonDialog, IBaseEditor
    {
        GEDCOMNoteRecord NoteRecord { get; set; }
    }

    public interface IPersonalNameEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMPersonalName PersonalName { get; set; }
    }

    public interface IPersonEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMIndividualRecord Person { get; set; }
        GEDCOMIndividualRecord Target { get; set; }
        TargetMode TargetMode { get; set; }
        void SetNeedSex(GEDCOMSex needSex);
    }

    public interface IRepositoryEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMRepositoryRecord Repository { get; set; }
    }

    public interface IResearchEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMResearchRecord Research { get; set; }
    }

    public interface ISexCheckDlg : ICommonDialog
    {
        string IndividualName { get; set; }
        GEDCOMSex Sex { get; set; }
    }

    public interface ISourceCitEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMSourceCitation SourceCitation { get; set; }
    }

    public interface ISourceEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMSourceRecord SourceRecord { get; set; }
    }

    public interface ITaskEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMTaskRecord Task { get; set; }
    }

    public interface IUserRefEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMUserReference UserRef { get; set; }
    }

    public interface IFilePropertiesDlg : ICommonDialog, IBaseEditor, IView
    {
    }

    public interface IPortraitSelectDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMMultimediaLink MultimediaLink { get; set; }
    }

    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor, IView
    {
        string FastFilter { get; set; }
        string Filter { get; set; }
        GEDCOMRecordType RecType { get; set; }
        TargetMode TargetMode { get; set; }
        GEDCOMIndividualRecord Target { get; set; }
        GEDCOMSex NeedSex { get; set; }
        GEDCOMRecord ResultRecord { get; set; }
    }

    public interface IDayTipsDlg : ICommonDialog
    {
        bool ShowTipsChecked { get; set; }

        void Init(string caption, bool showTipsChecked, StringList tips);
    }

    public interface ITreeFilterDlg : ICommonDialog, IView
    {
    }

    public interface ICircleChartWin : IView
    {
    }

    public interface ICommonFilterDlg : ICommonDialog, IView
    {
    }

    public interface IMapsViewerWin : ICommonDialog, IView
    {
    }

    public interface IPersonsFilterDlg : ICommonDialog, IView
    {
    }

    public interface IStatisticsWin : ICommonDialog, IView
    {
    }

    public interface ITreeChartWin : ICommonDialog, IView
    {
    }
}
