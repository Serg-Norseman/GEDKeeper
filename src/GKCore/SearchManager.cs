using System;
using System.Collections.Generic;

using GedCom551;
using GKCore.Sys;
using GKUI;
using Xapian;

namespace GKCore
{
	public class SearchManager
	{
		public SearchManager()
		{
		}

		public void ReindexBase(TfmBase aBase)
		{
			try
			{
				using (WritableDatabase database = new WritableDatabase(aBase.Engine.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
				using (TermGenerator indexer = new TermGenerator())
				using (Stem stemmer = new Xapian.Stem("russian"))
				{
					indexer.SetStemmer(stemmer);

					TfmProgress.ProgressInit(aBase.Tree.RecordsCount, "Индексация");

					int num = aBase.Tree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord rec = aBase.Tree.GetRecord(i);
						if (rec is TGEDCOMLocationRecord || rec is TGEDCOMGroupRecord) continue;

						using (Document doc = new Document())
						{
							TStrings ctx = aBase.GetRecordContext(rec);

							// при использовании слотов значений, база вырастает на 20%

							doc.SetData(rec.XRef);
							//doc.AddValue(0, rec.XRef);
							//doc.AddBooleanTerm();
							indexer.SetDocument(doc);
							indexer.IndexText(ctx.Text);
							database.AddDocument(doc);
						}
						TfmProgress.ProgressStep();
					}
					TfmProgress.ProgressDone();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.ReindexBase(): "+ex.Message);
			}
		}

		public class SearchEntry
		{
			public string XRef;
			public uint Rank;
			public int Percent;
		}

		public List<SearchEntry> Search(TfmBase aBase, string searchText)
		{
			List<SearchEntry> res = new List<SearchEntry>();

			try
			{
				using (Database database = new Database(aBase.Engine.GetXDBFolder()))
				using (Enquire enquire = new Enquire(database))
				using (Stem stemmer = new Stem("russian"))
				using (QueryParser qp = new QueryParser())
				{
					qp.SetStemmer(stemmer);
					qp.SetDatabase(database);
					qp.SetDefaultOp(Query.op.OP_AND);
					qp.SetStemmingStrategy(QueryParser.stem_strategy.STEM_SOME);
					uint flags = (uint)(QueryParser.feature_flag.FLAG_PARTIAL | QueryParser.feature_flag.FLAG_WILDCARD |
						QueryParser.feature_flag.FLAG_PHRASE | QueryParser.feature_flag.FLAG_BOOLEAN |
						QueryParser.feature_flag.FLAG_LOVEHATE);

					using (Query query = qp.ParseQuery(searchText, flags))
					{
						enquire.SetQuery(query);

						using (MSet matches = enquire.GetMSet(0, 100))
						{
							MSetIterator m = matches.Begin();
							while (m != matches.End())
							{
								try
								{
									SearchEntry entry = new SearchEntry();
									entry.XRef = m.GetDocument()/*.GetValue(0);*/.GetData();
									entry.Rank = m.GetRank()+1;
									entry.Percent = m.GetPercent();
									res.Add(entry);
								}
								catch (Exception ex)
								{
									SysUtils.LogWrite("SearchManager.Search(): " + ex.Message);
								}

								m = m.Next();
							}
						}
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.Search(): " + ex.Message);
			}

			return res;
		}

	}
}
