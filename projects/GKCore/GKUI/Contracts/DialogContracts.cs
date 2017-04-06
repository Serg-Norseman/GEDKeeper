/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Contracts
{
    public interface IAddressEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMAddress Address { get; set; }
    }

    public interface IAssociationEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMAssociation Association { get; set; }
    }

    public interface ICommunicationEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMCommunicationRecord Communication { get; set; }
    }

    public interface IEventEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMCustomEvent Event { get; set; }
    }

    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMFamilyRecord Family { get; set; }
        void SetTarget(FamilyTarget targetType, GEDCOMIndividualRecord target);
    }

    public interface IGroupEditDlg : ICommonDialog, IBaseEditor
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

    public interface ILocationEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMLocationRecord LocationRecord { get; set; }
    }

    public interface IMediaEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMMultimediaRecord MediaRec { get; set; }
    }

    public interface INameEditDlg : ICommonDialog, IBaseEditor
    {
        NameEntry IName { get; set; }
    }

    public interface INoteEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMNoteRecord NoteRecord { get; set; }
    }

    public interface INoteEditDlgEx : ICommonDialog, IBaseEditor
    {
        GEDCOMNoteRecord NoteRecord { get; set; }
    }

    public interface IPersonalNameEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMPersonalName PersonalName { get; set; }
    }

    public interface IPersonEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMIndividualRecord Person { get; set; }
        GEDCOMIndividualRecord Target { get; set; }
        TargetMode TargetMode { get; set; }
        void SetNeedSex(GEDCOMSex needSex);
    }

    public interface IRepositoryEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMRepositoryRecord Repository { get; set; }
    }

    public interface IResearchEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMResearchRecord Research { get; set; }
    }

    public interface ISexCheckDlg : ICommonDialog
    {
        string IndividualName { get; set; }
        GEDCOMSex Sex { get; set; }
    }

    public interface ISourceCitEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMSourceCitation SourceCitation { get; set; }
    }

    public interface ISourceEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMSourceRecord SourceRecord { get; set; }
    }

    public interface ITaskEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMTaskRecord Task { get; set; }
    }

    public interface IUserRefEditDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMUserReference UserRef { get; set; }
    }

    public interface IFilePropertiesDlg : ICommonDialog, IBaseEditor
    {
    }

    public interface IPortraitSelectDlg : ICommonDialog, IBaseEditor
    {
        GEDCOMMultimediaLink MultimediaLink { get; set; }
    }

    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor
    {
        string FastFilter { get; set; }
        string Filter { get; set; }
        GEDCOMRecordType Mode { get; set; }
        TargetMode TargetMode { get; set; }
        GEDCOMIndividualRecord Target { get; set; }
        GEDCOMSex NeedSex { get; set; }
        GEDCOMRecord ResultRecord { get; set; }
    }
}
