using System;

using GKCommon.GEDCOM;

namespace GKCore.Commands
{
	public class CmdPersonChangeBookmark : CustomCommand
	{
		private string fPersonXRef;
		private bool fOldVal;
		private bool fNewVal;

		public CmdPersonChangeBookmark(UndoManager manager, GEDCOMIndividualRecord person, bool newValue) : base(manager)
		{
			this.fPersonXRef = person.XRef;
			this.fOldVal = person.Bookmark;
			this.fNewVal = newValue;
		}

		public override bool Redo()
		{
			bool Result = true;
			GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
			if (i_rec == null)
			{
				Result = false;
			}
			else
			{
				i_rec.Bookmark = this.fNewVal;
			}
			return Result;
		}

		public override void Undo()
		{
			GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
			if (i_rec != null)
			{
				i_rec.Bookmark = this.fOldVal;
			}
		}
	}
}
