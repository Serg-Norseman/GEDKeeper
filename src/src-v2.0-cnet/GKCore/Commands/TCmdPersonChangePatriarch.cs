using GedCom551;
using GKCore;
using GKSys;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore.Commands
{
	public class TCmdPersonChangePatriarch : TCustomCommand
	{
		internal string FPersonXRef;
		internal bool FOldValue;
		internal bool FNewValue;

		public TCmdPersonChangePatriarch(TUndoManager aManager, TGEDCOMIndividualRecord aPerson, bool NewValue) : base(aManager)
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
