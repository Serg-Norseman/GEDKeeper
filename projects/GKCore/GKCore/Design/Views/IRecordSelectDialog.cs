/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public enum TargetMode
    {
        tmNone,
        tmParent,       // select child for parent
        tmChild,        // select parent for child
        tmSpouse,       // select spouse for indi
        tmFamilyChild,  // select family for child
        tmFamilySpouse, // select family with spouse for indi
    }


    public sealed class Target
    {
        public GDMIndividualRecord TargetIndividual;
        public TargetMode TargetMode;
        public GDMSex NeedSex;

        public Target()
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog
    {
        GDMRecord ResultRecord { get; set; }

        IComboBox FilterCombo { get; }
        ITextBox FilterText { get; }

        IFilterControl FilterCtl { get; }
        IListView RecordsList { get; }

        void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*");
    }
}
