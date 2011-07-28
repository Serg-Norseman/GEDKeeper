using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMFileReference : TGEDCOMTag
	{
		public enum TGEDCOMMultimediaFormat : byte
		{
			mfNone,
			mfBMP,
			mfGIF,
			mfJPG,
			mfOLE,
			mfPCX,
			mfTIF,
			mfWAV,
			mfTXT,
			mfRTF,
			mfAVI,
			mfTGA,
			mfPNG,
			mfMPG,
			mfHTM,
			mfUnknown
		}

		public enum TGEDCOMMediaType : byte
		{
			mtNone,
			mtAudio,
			mtBook,
			mtCard,
			mtElectronic,
			mtFiche,
			mtFilm,
			mtMagazine,
			mtManuscript,
			mtMap,
			mtNewspaper,
			mtPhoto,
			mtTombstone,
			mtVideo,
			mtUnknown
		}

		[Browsable(false)]
		public TGEDCOMFileReference.TGEDCOMMultimediaFormat MultimediaFormat
		{
			get
			{
				return this.GetMultimediaFormat();
			}
			set
			{
				this.SetMultimediaFormat(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMFileReference.TGEDCOMMediaType MediaType
		{
			get
			{
				return this.GetMediaType();
			}
			set
			{
				this.SetMediaType(value);
			}
		}
		internal TGEDCOMFileReference.TGEDCOMMultimediaFormat GetMultimediaFormat()
		{
			string S = base.GetTagStringValue("FORM").Trim().ToUpper();
			TGEDCOMFileReference.TGEDCOMMultimediaFormat Result;
			if (S.Equals(""))
			{
				Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfNone;
			}
			else
			{
				if (S.Equals("BMP"))
				{
					Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfBMP;
				}
				else
				{
					if (S.Equals("GIF"))
					{
						Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfGIF;
					}
					else
					{
						if (S.Equals("JPG"))
						{
							Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfJPG;
						}
						else
						{
							if (S.Equals("OLE"))
							{
								Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfOLE;
							}
							else
							{
								if (S.Equals("PCX"))
								{
									Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPCX;
								}
								else
								{
									if (S.Equals("TIF"))
									{
										Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTIF;
									}
									else
									{
										if (S.Equals("WAV"))
										{
											Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfWAV;
										}
										else
										{
											if (S.Equals("TXT"))
											{
												Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTXT;
											}
											else
											{
												if (S.Equals("RTF"))
												{
													Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfRTF;
												}
												else
												{
													if (S.Equals("AVI"))
													{
														Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfAVI;
													}
													else
													{
														if (S.Equals("TGA"))
														{
															Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTGA;
														}
														else
														{
															if (S.Equals("PNG"))
															{
																Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPNG;
															}
															else
															{
																if (S.Equals("MPG"))
																{
																	Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfMPG;
																}
																else
																{
																	if (S.Equals("HTM"))
																	{
																		Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfHTM;
																	}
																	else
																	{
																		Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfUnknown;
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}
		internal void SetMultimediaFormat([In] TGEDCOMFileReference.TGEDCOMMultimediaFormat Value)
		{
			switch (Value)
			{
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfBMP:
				{
					base.SetTagStringValue("FORM", "bmp");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfGIF:
				{
					base.SetTagStringValue("FORM", "gif");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfJPG:
				{
					base.SetTagStringValue("FORM", "jpg");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfOLE:
				{
					base.SetTagStringValue("FORM", "ole");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPCX:
				{
					base.SetTagStringValue("FORM", "pcx");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTIF:
				{
					base.SetTagStringValue("FORM", "tif");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfWAV:
				{
					base.SetTagStringValue("FORM", "wav");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTXT:
				{
					base.SetTagStringValue("FORM", "txt");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfRTF:
				{
					base.SetTagStringValue("FORM", "rtf");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfAVI:
				{
					base.SetTagStringValue("FORM", "avi");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTGA:
				{
					base.SetTagStringValue("FORM", "tga");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPNG:
				{
					base.SetTagStringValue("FORM", "png");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfMPG:
				{
					base.SetTagStringValue("FORM", "mpg");
					return;
				}
				case TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfHTM:
				{
					base.SetTagStringValue("FORM", "htm");
					return;
				}
			}
			base.SetTagStringValue("FORM", "");
		}
		internal TGEDCOMFileReference.TGEDCOMMediaType GetMediaType()
		{
			string S = base.GetTagStringValue(this.MediaTypeTagName()).Trim().ToUpper();
			TGEDCOMFileReference.TGEDCOMMediaType Result;
			if (BDSSystem.WStrCmp(S, "") == 0)
			{
				Result = TGEDCOMFileReference.TGEDCOMMediaType.mtNone;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "AUDIO") == 0)
				{
					Result = TGEDCOMFileReference.TGEDCOMMediaType.mtAudio;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "BOOK") == 0)
					{
						Result = TGEDCOMFileReference.TGEDCOMMediaType.mtBook;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "CARD") == 0)
						{
							Result = TGEDCOMFileReference.TGEDCOMMediaType.mtCard;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "ELECTRONIC") == 0)
							{
								Result = TGEDCOMFileReference.TGEDCOMMediaType.mtElectronic;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "FICHE") == 0)
								{
									Result = TGEDCOMFileReference.TGEDCOMMediaType.mtFiche;
								}
								else
								{
									if (BDSSystem.WStrCmp(S, "FILM") == 0)
									{
										Result = TGEDCOMFileReference.TGEDCOMMediaType.mtFilm;
									}
									else
									{
										if (BDSSystem.WStrCmp(S, "MAGAZINE") == 0)
										{
											Result = TGEDCOMFileReference.TGEDCOMMediaType.mtMagazine;
										}
										else
										{
											if (BDSSystem.WStrCmp(S, "MANUSCRIPT") == 0)
											{
												Result = TGEDCOMFileReference.TGEDCOMMediaType.mtManuscript;
											}
											else
											{
												if (BDSSystem.WStrCmp(S, "MAP") == 0)
												{
													Result = TGEDCOMFileReference.TGEDCOMMediaType.mtMap;
												}
												else
												{
													if (BDSSystem.WStrCmp(S, "NEWSPAPER") == 0)
													{
														Result = TGEDCOMFileReference.TGEDCOMMediaType.mtNewspaper;
													}
													else
													{
														if (BDSSystem.WStrCmp(S, "PHOTO") == 0)
														{
															Result = TGEDCOMFileReference.TGEDCOMMediaType.mtPhoto;
														}
														else
														{
															if (BDSSystem.WStrCmp(S, "TOMBSTONE") == 0)
															{
																Result = TGEDCOMFileReference.TGEDCOMMediaType.mtTombstone;
															}
															else
															{
																if (BDSSystem.WStrCmp(S, "VIDEO") == 0)
																{
																	Result = TGEDCOMFileReference.TGEDCOMMediaType.mtVideo;
																}
																else
																{
																	Result = TGEDCOMFileReference.TGEDCOMMediaType.mtUnknown;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}
		internal void SetMediaType([In] TGEDCOMFileReference.TGEDCOMMediaType Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMFileReference.TGEDCOMMediaType.mtAudio:
				{
					S = "audio";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtBook:
				{
					S = "book";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtCard:
				{
					S = "card";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtElectronic:
				{
					S = "electronic";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtFiche:
				{
					S = "fiche";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtFilm:
				{
					S = "film";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtMagazine:
				{
					S = "magazine";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtManuscript:
				{
					S = "manuscript";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtMap:
				{
					S = "map";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtNewspaper:
				{
					S = "newspaper";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtPhoto:
				{
					S = "photo";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtTombstone:
				{
					S = "tombstone";
					goto IL_AE;
				}
				case TGEDCOMFileReference.TGEDCOMMediaType.mtVideo:
				{
					S = "video";
					goto IL_AE;
				}
			}
			S = "";
			IL_AE:
			base.SetTagStringValue(this.MediaTypeTagName(), S);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FILE";
		}
		protected internal virtual string MediaTypeTagName()
		{
			return "FORM\\MEDI";
		}

		public void LinkFile([In] string AFile, TGEDCOMFileReference.TGEDCOMMediaType AMediaType, TGEDCOMFileReference.TGEDCOMMultimediaFormat AMultimediaFormat)
		{
			this.FStringValue = AFile;
			this.SetMultimediaFormat(TGEDCOMFileReference.RecognizeFormat(AFile));
			this.SetMediaType(AMediaType);
		}


		public static TGEDCOMFileReference.TGEDCOMMultimediaFormat RecognizeFormat([In] string AFile)
		{
			string E = Path.GetExtension(AFile).ToLower();
			TGEDCOMFileReference.TGEDCOMMultimediaFormat Result;
			if (E.Equals(".bmp"))
			{
				Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfBMP;
			}
			else
			{
				if (E.Equals(".gif"))
				{
					Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfGIF;
				}
				else
				{
					if (E.Equals(".jpg") || E.Equals(".jpeg"))
					{
						Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfJPG;
					}
					else
					{
						if (E.Equals(".ole"))
						{
							Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfOLE;
						}
						else
						{
							if (E.Equals(".pcx"))
							{
								Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPCX;
							}
							else
							{
								if (E.Equals(".tif") || E.Equals(".tiff"))
								{
									Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTIF;
								}
								else
								{
									if (E.Equals(".wav"))
									{
										Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfWAV;
									}
									else
									{
										if (E.Equals(".txt"))
										{
											Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTXT;
										}
										else
										{
											if (E.Equals(".rtf"))
											{
												Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfRTF;
											}
											else
											{
												if (E.Equals(".avi"))
												{
													Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfAVI;
												}
												else
												{
													if (E.Equals(".tga"))
													{
														Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfTGA;
													}
													else
													{
														if (E.Equals(".png"))
														{
															Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfPNG;
														}
														else
														{
															if (E.Equals(".mpg") || E.Equals(".mpeg"))
															{
																Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfMPG;
															}
															else
															{
																if (E.Equals(".htm") || E.Equals(".html"))
																{
																	Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfHTM;
																}
																else
																{
																	Result = TGEDCOMFileReference.TGEDCOMMultimediaFormat.mfUnknown;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		public TGEDCOMFileReference(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
