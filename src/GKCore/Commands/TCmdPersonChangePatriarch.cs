using System;

using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore.Commands
{
	public class TCmdPersonChangePatriarch : TCustomCommand
	{
		private string FPersonXRef;
		private bool FOldValue;
		private bool FNewValue;

		public TCmdPersonChangePatriarch(UndoManager aManager, TGEDCOMIndividualRecord aPerson, bool NewValue) : base(aManager)
		{
			this.FPersonXRef = aPerson.XRef;
			this.FOldValue = aPerson.Patriarch;
			this.FNewValue = NewValue;
		}

		public override bool Redo()
		{
			bool Result = true;
			TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FManager.Tree.XRefIndex_Find(this.FPersonXRef);
			if (i_rec == null)
			{
				Result = false;
			}
			else
			{
				i_rec.Patriarch = this.FNewValue;
			}
			return Result;
		}

		public override void Undo()
		{
			TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FManager.Tree.XRefIndex_Find(this.FPersonXRef);
			if (i_rec != null)
			{
				i_rec.Patriarch = this.FOldValue;
			}
		}
	}
}
