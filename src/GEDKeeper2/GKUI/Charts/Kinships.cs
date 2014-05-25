using System;

using ExtUtils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Charts
{
	public enum TRelationKind : byte
	{
		rkNone,		/* 0 */
		rkParent,	/* 1 */
		rkSpouse,	/* 2 */
		rkChild,	/* 3 */
		rkFather,	/* 4 */
		rkMother,	/* 5 */
		rkHusband,	/* 6 */
		rkWife,		/* 7 */
		rkSon,		/* 8 */
		rkDaughter,	/* 9 */
		rkGrandfather,
		rkGrandmother,
		rkGrandson,
		rkGranddaughter,
		rkBrother,
		rkSister,
		rkSonInLaw,
		rkDaughterInLaw,
		rkHusbandFather,
		rkHusbandMother,
		rkWifeFather,
		rkWifeMother,
		rkUncle,
		rkAunt,
		rkNephew,
		rkNiece,
		rkCousinM,
		rkCousinF,
		rkSame,
		rkUndefined
	}

	public static class KinshipsMan
	{
		public struct TKinshipRec
		{
			public EnumSet PrevRels;
			public EnumSet CurrRels;
			public TRelationKind FinRel;
			public sbyte Great;
			public sbyte Level;
		}

		public static TKinshipRec[] Kinships;

		static KinshipsMan()
		{
			InitKinships();
		}

		private static void RegisterKinship(EnumSet prevRels, EnumSet currRels, TRelationKind finRel, sbyte great, sbyte level)
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

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkNone
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkFather, 
				TRelationKind.rkMother, 
				TRelationKind.rkHusband, 
				TRelationKind.rkWife, 
				TRelationKind.rkSon, 
				TRelationKind.rkDaughter
			}), TRelationKind.rkSame, 0, 0);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkHusband, 
				TRelationKind.rkWife
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkSon, 
				TRelationKind.rkDaughter
			}), TRelationKind.rkSame, 0, 1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkMother
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkHusband
			}), TRelationKind.rkFather, 0, 0);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkFather
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkWife
			}), TRelationKind.rkMother, 0, 0);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkGrandfather, 
				TRelationKind.rkGrandmother
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkSon
			}), TRelationKind.rkUncle, 0, 1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkGrandfather, 
				TRelationKind.rkGrandmother
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkDaughter
			}), TRelationKind.rkAunt, 0, 1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkBrother, 
				TRelationKind.rkSister
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkSon
			}), TRelationKind.rkNephew, 0, 1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkBrother, 
				TRelationKind.rkSister
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkDaughter
			}), TRelationKind.rkNiece, 0, 1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkSon
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkWife
			}), TRelationKind.rkDaughterInLaw, 0, 0);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkDaughter
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkHusband
			}), TRelationKind.rkSonInLaw, 0, 0);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkWife
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkFather
			}), TRelationKind.rkWifeFather, 0, -1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkWife
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkMother
			}), TRelationKind.rkWifeMother, 0, -1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkHusband
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkFather
			}), TRelationKind.rkHusbandFather, 0, -1);

			RegisterKinship(EnumSet.Create(new Enum[]
			{
				TRelationKind.rkHusband
			}), EnumSet.Create(new Enum[]
			{
				TRelationKind.rkMother
			}), TRelationKind.rkHusbandMother, 0, -1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkFather }), 
				TRelationKind.rkGrandfather, 0, -1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkMother }), 
				TRelationKind.rkGrandmother, 0, -1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkSon }), 
				TRelationKind.rkBrother, 0, 1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkFather, TRelationKind.rkMother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkSister, 0, 1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkGrandfather, TRelationKind.rkGrandmother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkFather }), 
				TRelationKind.rkGrandfather, 1, -1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkGrandfather, TRelationKind.rkGrandmother }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkMother }), 
				TRelationKind.rkGrandmother, 1, -1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkSon, TRelationKind.rkDaughter, TRelationKind.rkSonInLaw, TRelationKind.rkDaughterInLaw }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkSon }), TRelationKind.rkGrandson, 0, 1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkSon, TRelationKind.rkDaughter, TRelationKind.rkSonInLaw, TRelationKind.rkDaughterInLaw }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkGranddaughter, 0, 1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkGrandson, TRelationKind.rkGranddaughter }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkSon }), 
				TRelationKind.rkGrandson, 1, 1);

			RegisterKinship(
				EnumSet.Create(new Enum[] { TRelationKind.rkGrandson, TRelationKind.rkGranddaughter }), 
				EnumSet.Create(new Enum[] { TRelationKind.rkDaughter }), 
				TRelationKind.rkGranddaughter, 1, 1);
		}

		public static TRelationKind FindKinship(TRelationKind prev, TRelationKind cur, out int great, out int level)
		{
			TRelationKind Result = TRelationKind.rkUndefined;
			great = 0;
			level = 0;

			int num = Kinships.Length - 1;
			for (int i = 0; i <= num; i++)
			{
				TKinshipRec kinship = Kinships[i];

				if (kinship.PrevRels.InSet(prev) && kinship.CurrRels.InSet(cur))
				{
					TRelationKind rel = kinship.FinRel;
					great = (int)kinship.Great;
					level = (int)kinship.Level;

					if (rel == TRelationKind.rkSame)
					{
						rel = cur;
					}

					Result = rel;
				}
			}

			return Result;
		}

	}
}
