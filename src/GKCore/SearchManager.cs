using System;
using System.Collections.Generic;
using System.IO;

using Ext.Utils;
using GedCom551;
using GKUI;
using Xapian;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class SearchManager
	{
		private object xdb_lock = new object();

		public SearchManager()
		{
		}

		private string GetSign(TfmBase aBase)
		{
			return Path.GetFileNameWithoutExtension(aBase.Tree.FileName);
		}

		private bool IsIndexedRecord(TGEDCOMRecord rec)
		{
			return !((rec is TGEDCOMLocationRecord || rec is TGEDCOMGroupRecord));
		}

		private void SetDBLastChange(TfmBase FBase, WritableDatabase database)
		{
			string db_lastchange = FBase.Tree.Header.TransmissionDateTime.ToString("yyyy.MM.dd HH:mm:ss", null);
			database.SetMetadata(GetSign(FBase), db_lastchange);
		}

		private string GetXDBFolder()
		{
			string xdb_dir = GKUtils.GetAppDataPath() + "xdb";
			if (!Directory.Exists(xdb_dir)) Directory.CreateDirectory(xdb_dir);
			return xdb_dir;
		}

		private uint FindDocId(TfmBase FBase, WritableDatabase database, string xref)
		{
			uint result;

			string key = "Q" + GetSign(FBase) + "_" + xref;

			PostingIterator p = database.PostListBegin(key);
			if (p == database.PostListEnd(key)) {
				result = 0; // 0 - is invalid docid (see XapianManual)
			} else {
				result = p.GetDocId();
			}

			return result;
		}

		private void SetDocumentContext(TfmBase FBase, Document doc, TermGenerator indexer, TGEDCOMRecord rec)
		{
			StringList ctx = FBase.GetRecordContext(rec);
			string rec_lastchange = rec.ChangeDate.ToString();
			string base_sign = GetSign(FBase);

			doc.SetData(rec.XRef);							// not edit: for link from search results to gedcom-base
			doc.AddTerm("Q" + base_sign + "_" + rec.XRef);	// not edit: specific db_rec_id - for FindDocId()
			doc.AddValue(0, rec_lastchange);				// not edit: for update check
			doc.AddBooleanTerm("GDB" + base_sign);			// not edit: for filtering by database in Search()

			indexer.SetDocument(doc);
			indexer.IndexText(ctx.Text);
		}

		private void ReindexRecord(TfmBase FBase, WritableDatabase database, TermGenerator indexer, TGEDCOMRecord record)
		{
			uint docid = FindDocId(FBase, database, record.XRef);

			if (docid != 0) {
				// проверка на необходимость обновления
				string rec_lastchange = record.ChangeDate.ToString();

				Document cur_doc = database.GetDocument(docid);
				string doc_lastchange = cur_doc.GetValue(0);

				// обновление записи
				if (!string.Equals(rec_lastchange, doc_lastchange)) {
					using (Document doc = new Document())
					{
						SetDocumentContext(FBase, doc, indexer, record);
						database.ReplaceDocument(docid, doc);
					}
				}
			} else {
				// чистое добавление
				using (Document doc = new Document())
				{
					SetDocumentContext(FBase, doc, indexer, record);
					database.AddDocument(doc);
				}
			}
		}

		public void ReindexBase(TfmBase FBase)
		{
			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(this.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Xapian.Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						TfmProgress.ProgressInit(FBase.Tree.RecordsCount, LangMan.LS(LSID.LSID_SearchIndexRefreshing));
						int num = FBase.Tree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord record = FBase.Tree[i];
							if (IsIndexedRecord(record)) ReindexRecord(FBase, database, indexer, record);
							TfmProgress.ProgressStep();
						}
						TfmProgress.ProgressDone();

						SetDBLastChange(FBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.ReindexBase(): " + ex.Message);
			}
		}

		public void UpdateRecord(TfmBase FBase, TGEDCOMRecord record)
		{
			if (record == null || !IsIndexedRecord(record)) return;

			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(this.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Xapian.Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						ReindexRecord(FBase, database, indexer, record);
						SetDBLastChange(FBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.UpdateRecord(): " + ex.Message);
			}
		}

		public void DeleteRecord(TfmBase FBase, string xref)
		{
			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(this.GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					{
						uint docid = FindDocId(FBase, database, xref);
						if (docid != 0) {
							database.DeleteDocument(docid);
							SetDBLastChange(FBase, database);
						}
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.DeleteRecord(): " + ex.Message);
			}
		}

		public class SearchEntry
		{
			public string XRef;
			public uint Rank;
			public int Percent;
		}

		public List<SearchEntry> Search(TfmBase FBase, string searchText)
		{
			List<SearchEntry> res = new List<SearchEntry>();

			try
			{
				lock (xdb_lock)
				{
					using (Database database = new Database(this.GetXDBFolder()))
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

						string qs = searchText + " ged:" + GetSign(FBase);
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
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("SearchManager.Search(): " + ex.Message);
			}

			return res;
		}

	}
}
