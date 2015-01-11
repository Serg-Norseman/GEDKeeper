using System;

using ExtUtils;
using GKCore;

/// <summary>
/// 
/// </summary>

namespace GKUI.Charts
{
	using TRelationSet = EnumSet<TRelationKind>;

	public static class KinshipsMan
	{
		public struct TKinshipRec
		{
			public TRelationSet PrevRels;
			public TRelationSet CurrRels;
			public TRelationKind FinRel;
			public sbyte Great;
			public sbyte Level;
		}

		public static TKinshipRec[] Kinships;

		static KinshipsMan()
		{
			InitKinships();
		}

		private static void RegisterKinship(TRelationSet prevRels, TRelationSet currRels, TRelationKind finRel, sbyte great, sbyte level)
		{
			int len = (Kinships == null) ? 0 : Kinships.Length;
			int num = len + 1;

			TKinshipRec[] new_array = new TKinshipRec[num];

			if (num > 0 && Kinships != null)
			{
				int num2 = Kinships.Length;
				if (num2 > 0) Array.Copy(Kinships, new_array, num2);
			}

			Kinships = new_array;
			Kinships[len].PrevRels = prevRels;
			Kinships[len].CurrRels = currRels;
			Kinships[len].FinRel = finRel;
			Kinships[len].Great = great;
			Kinships[len].Level = level;
		}

		private static void InitKinships()
		{
			Kinships = new TKinshipRec[0];

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkNone
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkFather, 
				TRelationKind.rkMother, 
				TRelationKind.rkHusband, 
				TRelationKind.rkWife, 
				TRelationKind.rkSon, 
				TRelationKind.rkDaughter
			}), TRelationKind.rkSame, 0, 0);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkHusband, 
				TRelationKind.rkWife
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkSon, 
				TRelationKind.rkDaughter
			}), TRelationKind.rkSame, 0, 1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkMother
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkHusband
			}), TRelationKind.rkFather, 0, 0);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkFather
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkWife
			}), TRelationKind.rkMother, 0, 0);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkGrandfather, 
				TRelationKind.rkGrandmother
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkSon
			}), TRelationKind.rkUncle, 0, 1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkGrandfather, 
				TRelationKind.rkGrandmother
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkDaughter
			}), TRelationKind.rkAunt, 0, 1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkBrother, 
				TRelationKind.rkSister
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkSon
			}), TRelationKind.rkNephew, 0, 1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkBrother, 
				TRelationKind.rkSister
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkDaughter
			}), TRelationKind.rkNiece, 0, 1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkSon
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkWife
			}), TRelationKind.rkDaughterInLaw, 0, 0);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkDaughter
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkHusband
			}), TRelationKind.rkSonInLaw, 0, 0);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkWife
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkFather
			}), TRelationKind.rkWifeFather, 0, -1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkWife
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkMother
			}), TRelationKind.rkWifeMother, 0, -1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkHusband
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkFather
			}), TRelationKind.rkHusbandFather, 0, -1);

			RegisterKinship(TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkHusband
			}), TRelationSet.Create(new TRelationKind[]
			{
				TRelationKind.rkMother
			}), TRelationKind.rkHusbandMother, 0, -1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather }), 
				TRelationKind.rkGrandfather, 0, -1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkMother }), 
				TRelationKind.rkGrandmother, 0, -1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkSon }), 
				TRelationKind.rkBrother, 0, 1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkSister, 0, 1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkGrandfather, TRelationKind.rkGrandmother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkFather }), 
				TRelationKind.rkGrandfather, 1, -1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkGrandfather, TRelationKind.rkGrandmother }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkMother }), 
				TRelationKind.rkGrandmother, 1, -1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkSon, TRelationKind.rkDaughter, TRelationKind.rkSonInLaw, TRelationKind.rkDaughterInLaw }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkSon }), TRelationKind.rkGrandson, 0, 1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkSon, TRelationKind.rkDaughter, TRelationKind.rkSonInLaw, TRelationKind.rkDaughterInLaw }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkGranddaughter, 0, 1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkGrandson, TRelationKind.rkGranddaughter }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkSon }), 
				TRelationKind.rkGrandson, 1, 1);

			RegisterKinship(
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkGrandson, TRelationKind.rkGranddaughter }), 
				TRelationSet.Create(new TRelationKind[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkGranddaughter, 1, 1);
		}

		public static TRelationKind FindKinship(TRelationKind prev, TRelationKind cur, out int great, out int level)
		{
			TRelationKind result = TRelationKind.rkUndefined;
			great = 0;
			level = 0;

			int num = Kinships.Length;
			for (int i = 0; i < num; i++)
			{
				TKinshipRec kinship = Kinships[i];

				if (kinship.PrevRels.Contains(prev) && kinship.CurrRels.Contains(cur)) {
					TRelationKind rel = kinship.FinRel;
					great = (int)kinship.Great;
					level = (int)kinship.Level;

					if (rel == TRelationKind.rkSame) {
						rel = cur;
					}

					result = rel;
				}
			}

			return result;
		}

	}
}
