using System;

using GKCommon.GEDCOM;

namespace GKCore.Commands
{
	public class CmdPersonChangePatriarch : CustomCommand
	{
		private string fPersonXRef;
		private bool fOldVal;
		private bool fNewVal;

		public CmdPersonChangePatriarch(UndoManager manager, GEDCOMIndividualRecord person, bool newValue) : base(manager)
		{
			this.fPersonXRef = person.XRef;
			this.fOldVal = person.Patriarch;
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
				i_rec.Patriarch = this.fNewVal;
			}
			return Result;
		}

		public override void Undo()
		{
			GEDCOMIndividualRecord i_rec = this.fManager.Tree.XRefIndex_Find(this.fPersonXRef) as GEDCOMIndividualRecord;
			if (i_rec != null)
			{
				i_rec.Patriarch = this.fOldVal;
			}
		}
	}
}
