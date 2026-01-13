/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RecordInfoDlgController : DialogController<IRecordInfoDlg>
    {
        private GDMRecord fRecord;

        public GDMRecord Record
        {
            get { return fRecord; }
            set {
                if (fRecord != value) {
                    fRecord = value;
                    UpdateView();
                }
            }
        }


        public RecordInfoDlgController(IRecordInfoDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
            fView.HyperView.Lines.Assign(fBase.GetRecordContent(fRecord, RecordContentType.Quick));
        }

        public void SelectLink(string linkName)
        {
            if (linkName.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                GKUtils.LoadExtFile(linkName);
                return;
            }

            if (linkName.StartsWith(GKData.INFO_HREF_VIEW)) {
                fBase.ShowMedia(linkName, false);
            } else {
                var record = Base.Context.Tree.FindXRef<GDMRecord>(linkName);
                fView.HyperView.Lines.Assign(fBase.GetRecordContent(record, RecordContentType.Quick));
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIFileProperties));
        }

        public override void ApplyTheme()
        {
            // dummy
        }
    }
}
