using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public abstract class GKCustomSheet : GKSheetList
	{
    	private readonly IBaseEditor fBaseEditor;
        private IGEDCOMListEnumerator fDataList;

        public IGEDCOMListEnumerator DataList
        {
            get { return this.fDataList; }
            set { 
                this.fDataList = value;
                this.UpdateSheet();
            }
        }

        public IBaseEditor Editor
        {
        	get { return this.fBaseEditor; }
        }
        
        protected GKCustomSheet(IBaseEditor baseEditor, Control owner) : base(owner)
        {
        	this.fBaseEditor = baseEditor;
            this.fDataList = null;
        }

        public abstract void UpdateSheet();
	}
}
