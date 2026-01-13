/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Lists;

namespace GKCore.Design.Views
{
    public interface IPersonEditDlg : ICommonDialog
    {
        GDMIndividualRecord IndividualRecord { get; set; }
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
        ITextBox Nickname { get; }
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
    }
}
