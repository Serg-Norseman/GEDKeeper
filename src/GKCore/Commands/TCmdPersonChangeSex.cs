using System;

using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore.Commands
{
	public class TCmdPersonChangeSex : TCustomCommand
	{
		private string FPersonXRef;
		private TGEDCOMSex FOldSex;
		private TGEDCOMSex FNewSex;

		public TCmdPersonChangeSex(UndoManager aManager, TGEDCOMIndividualRecord aPerson, TGEDCOMSex NewSex) : base(aManager)
		{
			this.FPersonXRef = aPerson.XRef;
			this.FOldSex = aPerson.Sex;
			this.FNewSex = NewSex;
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
				i_rec.Sex = this.FNewSex;
			}
			return Result;
		}

		public override void Undo()
		{
			TGEDCOMIndividualRecord i_rec = (TGEDCOMIndividualRecord)this.FManager.Tree.XRefIndex_Find(this.FPersonXRef);
			if (i_rec != null)
			{
				i_rec.Sex = this.FOldSex;
			}
		}
	}
}
