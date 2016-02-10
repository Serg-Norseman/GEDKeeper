using System;
using GKCommon.GEDCOM;

namespace GKCore.Stats
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class CompositeItem
	{
		private float CommonSum;
		private float MaleSum;
		private float FemaleSum;

		private int CommonCount;
		private int MaleCount;
		private int FemaleCount;

		public double CommonVal { get { return GKUtils.SafeDiv(CommonSum, CommonCount); } }
		public double MaleVal { get { return GKUtils.SafeDiv(MaleSum, MaleCount); } }
		public double FemaleVal { get { return GKUtils.SafeDiv(FemaleSum, FemaleCount); } }

		public CompositeItem()
		{
			this.CommonSum = 0;
			this.MaleSum = 0;
			this.FemaleSum = 0;

			this.CommonCount = 0;
			this.MaleCount = 0;
			this.FemaleCount = 0;
		}

		public void TakeVal(float val, GEDCOMSex sex, bool ignoreZero)
		{
			if (val == 0 && ignoreZero) return;

			CommonSum += val;
			CommonCount++;
			
			switch (sex) {
				case GEDCOMSex.svFemale:
					FemaleSum += val;
					FemaleCount++;
					break;

				case GEDCOMSex.svMale:
					MaleSum += val;
					MaleCount++;
					break;
			}
		}

		public void TakeVal(string val, GEDCOMSex sex, bool ignoreZero)
		{
			int tmp;
			if (int.TryParse(val, out tmp))
			{
				TakeVal(tmp, sex, ignoreZero);
			}
		}
	}
}
