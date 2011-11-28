using System;
using System.Collections.Generic;
using System.IO;

using GedCom551;
using GKCore.Sys;
using GKUI;
using Xapian;

namespace GKCore
{
	public class SearchManager
	{
		private TfmBase FBase;
		private string FBaseSign;

		public SearchManager(TfmBase aBase)
		{
			this.FBase = aBase;
			this.FBaseSign = Path.GetFileNameWithoutExtension(FBase.Engine.FileName);
		}

		private bool IsNotIndexedRecord(TGEDCOMRecord rec)
		{
			return (rec is TGEDCOMLocationRecord || rec is TGEDCOMGroupRecord);
		}

		private string GetDBLastChangeDateTime()
		{
			return FBase.Tree.Header.TransmissionDateTime.ToString("yyyy.MM.dd HH:mm:ss", null);
		}

		private uint FindDocId(WritableDatabase database, string xref)
		{
			uint result;

			string key = "Q" + FBaseSign + "_" + xref;

			PostingIterator p = database.PostListBegin(key);
			if (p == database.PostListEnd(key)) {
				result = 0; // 0 - is invalid docid (see XapianManual)
			} else {
				result = p.GetDocId();
			}

			return result;
		}

		public bool IsIndexed()
		{
			bool result;

			using (WritableDatabase database = new WritableDatabase(FBase.Engine.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
			{
				string lastchanged = database.GetMetadata(FBaseSign);
				result = !string.IsNullOrEmpty(lastchanged);
			}

			return result;
		}

		private void SetDocumentContext(Document doc, TermGenerator indexer, TGEDCOMRecord rec)
		{
			TStrings ctx = FBase.GetRecordContext(rec);
			string rec_lastchange = rec.ChangeDate.ToString();

			doc.SetData(rec.XRef); // not edit: for link from search to dbrecords
			doc.AddTerm("Q" + FBaseSign + "_" + rec.XRef); // database specific record id - for edit&delete
			doc.AddValue(0, rec_lastchange); // for update check
			doc.AddBooleanTerm("GDB" + FBaseSign); // not edit: for filtering by database

			indexer.SetDocument(doc);
			indexer.IndexText(ctx.Text);
		}

		public void ReindexBase()
		{
			try
			{
				using (WritableDatabase database = new WritableDatabase(FBase.Engine.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
				using (TermGenerator indexer = new TermGenerator())
				using (Stem stemmer = new Xapian.Stem("russian"))
				{
					indexer.SetStemmer(stemmer);

					TfmProgress.ProgressInit(FBase.Tree.RecordsCount, "Индексация");
					int num = FBase.Tree.RecordsCount - 1;
					for (int i = 0; i <= num; i++)
					{
						TGEDCOMRecord temp_rec = FBase.Tree.GetRecord(i);
						if (IsNotIndexedRecord(temp_rec)) continue;

						using (Document doc = new Document())
						{
							SetDocumentContext(doc, indexer, temp_rec);
							database.AddDocument(doc);
						}

						TfmProgress.ProgressStep();
					}
					TfmProgress.ProgressDone();

					database.SetMetadata(FBaseSign, GetDBLastChangeDateTime());
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.ReindexBase(): "+ex.Message);
			}
		}

		public void ftsUpdateRecord(TGEDCOMRecord record)
		{
			if (record == null || IsNotIndexedRecord(record)) return;

			try
			{
				using (WritableDatabase database = new WritableDatabase(FBase.Engine.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
				using (TermGenerator indexer = new TermGenerator())
				using (Stem stemmer = new Xapian.Stem("russian"))
				{
					indexer.SetStemmer(stemmer);

					uint docid = FindDocId(database, record.XRef);

					using (Document doc = new Document())
					{
						SetDocumentContext(doc, indexer, record);
						database.ReplaceDocument(docid, doc);
					}

					database.SetMetadata(FBaseSign, GetDBLastChangeDateTime());
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.ftsUpdateRecord(): "+ex.Message);
			}
		}

		public void ftsDeleteRecord(string xref)
		{
			try
			{
				using (WritableDatabase database = new WritableDatabase(FBase.Engine.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
				{
					uint docid = FindDocId(database, xref);
					if (docid != 0) {
						database.DeleteDocument(docid);

						database.SetMetadata(FBaseSign, GetDBLastChangeDateTime());
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.ftsDeleteRecord(): "+ex.Message);
			}
		}

		public class SearchEntry
		{
			public string XRef;
			public uint Rank;
			public int Percent;
		}

		public List<SearchEntry> Search(string searchText)
		{
			List<SearchEntry> res = new List<SearchEntry>();

			try
			{
				using (Database database = new Database(FBase.Engine.GetXDBFolder()))
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

					string qs = searchText + " ged:" + FBaseSign;
					qp.AddBooleanPrefix("ged", "GDB");

					using (Query query = qp.ParseQuery(qs, flags))
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
									entry.XRef = m.GetDocument().GetData();
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
