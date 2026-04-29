# Справочник MCP-инструментов GKcli

## Обзор

**GKcli** — это CLI-приложение GEDKeeper с поддержкой протокола [MCP](https://modelcontextprotocol.io/),
позволяющее LLM-клиентам взаимодействовать с генеалогической базой данных через набор инструментов.

**Запуск в LLM-клиентах:** `GKcli --mcp` (аргумент обязателен, т.к. без него запускается интерактивный текстовый терминал, частично повторяющий по составу функций MCP-сервер).

**Протокол:** MCP `2025-06-18`, JSON-RPC 2.0 поверх stdin/stdout. Без внешних зависимостей.

---

## LM-клиенты

- **Jan**: подтверждено успешное стабильное функционирование MCP-сервера с локальными и облачными моделями (с маркировкой использования инструментов).
- **LM Studio**: клиент каждые 1-2 минуты перезапускает новый экземпляр MCP-сервера, что приводит к непрерывным сбросам контекста БД.
- **Msty**: клиент нормально не функционирует.
- **GPT4All**: MCP-протокол совсем не поддерживается.

---

## Особенности реализации

- **Автосоздание бэкапов:** при работе через MCP автоматически включается принудительное резервное копирование каждого файла при сохранении.
- **Пагинация:** таблицы с >20 записей разбиваются постранично; в конце ответа подсказка для запроса следующей страницы.
- **Нечёткий поиск:** `individual_search` и `record_search` используют алгоритм нечёткого сопоставления с настраиваемым порогом.
- **XRef-идентификаторы записей:** все идентификаторы автоматически генерируются при добавлении новых записей. Уникальность контролируется ядром комплекса приложений GEDKeeper.
- **Обработка ошибок:** все ошибки, которые могут возникнуть в процессе работы - отправляются в человеко-читаемом виде из инструментов в LM-клиент для отображения пользователю.
- **Поддержка локализаций**: по причине работы GKcli/MCP как части комплекса приложений GEDKeeper, инструмент использует те-же файлы и подходы к локализации, что и основное GUI-приложение. По умолчанию, при отсутствии специальной настройки, инструмент будет использовать английский язык взаимодействия. Настройку можно выполнить, запустив GKcli из командной строки без ключа `--mcp` - при этом будет доступен раздел настроек.
- **Формат данных и валидация входных данных**: формат данных подробно описывается в дескрипторах инструментов внутри MCP-сервера, в них же выполняется окончательная валидация, после подготовки данных моделью.

---

## Файловые операции

| Инструмент | Описание | Параметры |
|---|---|---|
| `file_new` | Создать новую пустую базу GEDCOM | — |
| `file_load` | Загрузить GEDCOM-файл | `path` (string) — путь к `.ged` |
| `file_save` | Сохранить базу в GEDCOM-файл | `path` (string) — путь для сохранения |
| `file_props` | Получить информацию о текущей базе (автор, адрес, статистика записей) | — |
| `file_recent` | Список недавно открытых файлов | — |
| `file_reload` | Перезагрузить последний открытый файл | — |
| `file_search` | Найти все GEDCOM-файлы на диске | `path` (string) — корневой каталог для поиска |
| `file_validate` | Проверить валидность текущей базы | — |
| `file_merge` | Влить другой GEDCOM файл в текущую базу данных | `path` (string) — путь к `.ged` |

---

## Записи (общее)

| Инструмент | Описание | Параметры |
|---|---|---|
| `record_list` | Список всех записей указанного типа (пагинация, 20 на стр.) | `record_type` (string), `page` (integer, по умолч. 1) |
| `record_delete` | Удалить запись | `xref` (string, напр. `I1`) |
| `record_merge` | Объединить записи | `target_xref` (string, напр. `I1`), `source_xref` (string, напр. `I2`) |
| `record_info` | Получить полную информацию о записи | `xref` (string, напр. `I1`) |
| `record_search` | Нечёткий поиск по любым записям (имя/заголовок) | `record_type` (string), `search_text` (string), `threshold` (number, по умолч. 0.15) |
| `record_set_restriction` | Установить ограничение доступа/видимости для записи | `xref` (string), `restriction` (string: `None`, `Locked`, `Confidential`, `Privacy`) |
| `record_list_userrefs` | Список пользовательских сносок записи | `record_xref` (string) |
| `record_add_userref` | Добавить пользовательскую сноску к записи | `record_xref` (string), `string_value` (string), `reference_type` (string, необязательно) |
| `record_delete_userref` | Удалить пользовательскую сноску из записи | `record_xref` (string), `reference_index` (integer, 0-based) |
| `record_list_sources` | Список цитат источников записи | `record_xref` (string) |
| `record_add_source` | Добавить цитату источника к записи | `record_xref` (string), `source_xref` (string), `page` (string, необязательно), `certainty` (integer, 0–3, необязательно) |
| `record_delete_source` | Удалить цитату источника из записи | `record_xref` (string), `citation_index` (integer, 0-based) |
| `record_list_multimedia` | Список мультимедиа-объектов записи | `record_xref` (string) |
| `record_add_multimedia` | Добавить ссылку на мультимедиа к записи | `record_xref` (string), `multimedia_xref` (string), `is_primary` (boolean, по умолч. false) |
| `record_delete_multimedia` | Удалить ссылку на мультимедиа из записи | `record_xref` (string), `link_index` (integer, 0-based) |
| `record_list_notes` | Список заметок записи | `record_xref` (string) |
| `record_add_note` | Добавить ссылку на заметку к записи | `record_xref` (string), `note_xref` (string) |
| `record_delete_note` | Удалить ссылку на заметку из записи | `record_xref` (string), `note_index` (integer, 0-based) |
| `event_type_list` | Список всех доступных типов событий для персон или семей | `record_type` (string) |

**Типы записей:** `Individual`, `Family`, `Note`, `Source`, `Repository`, `Multimedia`, `Group`, `Task`, `Research`, `Communication`, `Location`

---

## Персоны (Individuals)

| Инструмент | Описание | Параметры |
|---|---|---|
| `individual_search` | Нечёткий поиск по имени (порог 16%) | `name` (string) |
| `individual_upsert` | Добавить персону или редактировать существующую | `xref` (string, необязательно), `name` (string), `sex` (string: `m`/`f`), `nickname` (string, необязательно) |
| `individual_list_associations` | Список всех ассоциаций персоны | `individual_xref` (string) |
| `individual_upsert_association` | Добавить ассоциацию персоне или редактировать существующую | `individual_xref` (string), `association_index` (integer, 0-based), `associate_xref` (string), `relation` (string) |
| `individual_delete_association` | Удалить ассоциацию у персоны | `individual_xref` (string), `association_index` (integer, 0-based) |
| `individual_list_events` | Список всех событий персоны | `individual_xref` (string) |
| `individual_delete_event` | Удалить событие персоны | `individual_xref` (string), `event_index` (integer, 0-based) |
| `individual_upsert_event` | Добавить событие персоне или редактировать существующее | `individual_xref` (string), `event_index` (integer, 0-based), `type` (string), `date` (string), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string), `age` (string) |
| `individual_list_personal_names` | Список всех персональных имён персоны | `individual_xref` (string) |
| `individual_upsert_personal_name` | Добавить персональное имя или редактировать существующее | `individual_xref` (string), `name_index` (integer, 0-based), `given` (string), `surname` (string), `surname_prefix` (string), `name_prefix` (string), `name_suffix` (string), `nickname` (string), `name_type` (string), `language` (string), `patronymic` (string), `married_name` (string), `religious_name` (string), `census_name` (string) |
| `individual_delete_personal_name` | Удалить персональное имя персоны | `individual_xref` (string), `name_index` (integer, 0-based) |
| `individual_list_spouses` | Список всех супругов персоны | `individual_xref` (string) |
| `individual_list_groups` | Список всех групп персоны | `individual_xref` (string) |

---

## Семьи (Families)

| Инструмент | Описание | Параметры |
|---|---|---|
| `family_upsert` | Создать семью или редактировать существующую | `xref` (string, необязательно), `husband_xref` (string), `wife_xref` (string) |
| `family_list_children` | Список детей семьи | `family_xref` (string) |
| `family_add_child` | Добавить ребёнка в семью | `family_xref` (string), `child_xref` (string), `linkage_type` (string, необязательно) |
| `family_delete_child` | Удалить ребёнка из семьи | `family_xref` (string), `child_xref` (string) |
| `family_list_events` | Список всех событий семьи | `family_xref` (string) |
| `family_delete_event` | Удалить событие семьи | `family_xref` (string), `event_index` (integer, 0-based) |
| `family_upsert_event` | Добавить событие семье или редактировать существующее | `family_xref` (string), `event_index` (integer, 0-based), `type` (string), `date` (string), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string), `husband_age` (string), `wife_age` (string) |

---

## Заметки (Notes)

| Инструмент | Описание | Параметры |
|---|---|---|
| `note_upsert` | Добавить заметку или редактировать существующую | `xref` (string, необязательно), `text` (string) |

---

## Источники (Sources)

| Инструмент | Описание | Параметры |
|---|---|---|
| `source_upsert` | Добавить источник или редактировать существующий | `xref` (string, необязательно), `title` (string), `short_title` (string, необязательно), `author` (string, необязательно) |
| `source_list_repositories` | Список ссылок на архивы | `source_xref` (string) |
| `source_add_repository` | Добавить ссылку на архив в источник | `source_xref` (string), `repository_xref` (string) |
| `source_delete_repository` | Удалить ссылку на архив из источника | `source_xref` (string), `repository_xref` (string) |

---

## Хранилища/архивы (Repositories)

| Инструмент | Описание | Параметры |
|---|---|---|
| `repository_upsert` | Добавить хранилище/архив или редактировать существующее | `xref` (string), `name` (string) |

---

## Мультимедиа (Multimedia)

| Инструмент | Описание | Параметры |
|---|---|---|
| `multimedia_upsert` | Добавить мультимедиа-запись с файловой ссылкой или редактировать существующую | `xref` (string), `title` (string), `file_path` (string), `media_type` (string), `store_type` (string) |
| `multimedia_get` | Получить мультимедиа-запись | `xref` (string, напр. `O1`) |
| `multimedia_list_files` | Список файлов в мультимедиа-записи | `xref` (string) |
| `multimedia_upsert_file` | Добавить файл в мультимедиа-запись или редактировать существующий | `xref` (string), `file_index` (integer, 0-based), `title` (string), `file_path` (string), `media_type` (string), `store_type` (string) |
| `multimedia_delete_file` | Удалить файл из мультимедиа-записи | `xref` (string), `file_index` (string) |

---

## Группы (Groups)

| Инструмент | Описание | Параметры |
|---|---|---|
| `group_upsert` | Создать группу или редактировать существующую | `xref` (string), `name` (string) |
| `group_list_members` | Список членов группы | `group_xref` (string) |
| `group_add_member` | Добавить персону в группу | `group_xref` (string), `individual_xref` (string) |
| `group_delete_member` | Удалить персону из группы | `group_xref` (string), `individual_xref` (string) |

---

## Задачи (Tasks)

| Инструмент | Описание | Параметры |
|---|---|---|
| `task_upsert` | Добавить задачу или редактировать существующую | `xref` (string, необязательно), `goal` (string), `priority` (enum), `start_date` (string, необязательно), `stop_date` (string, необязательно) |

---

## Исследования (Researches)

| Инструмент | Описание | Параметры |
|---|---|---|
| `research_upsert` | Добавить исследование или редактировать существующее | `xref` (string), `title` (string), `priority` (enum), `status` (enum), `start_date` (string), `stop_date` (string), `percent` (integer) |
| `research_list_tasks` | Список всех задач исследования по его XRef идентификатору | `research_xref` (string) — XRef идентификатор исследования (например, 'R1') |
| `research_add_task` | Добавить задачу в исследование по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `task_xref` (string) — XRef идентификатор задачи (например, 'T1', 'T2') |
| `research_delete_task` | Удалить задачу из исследования по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `task_xref` (string) — XRef идентификатор задачи (например, 'T1', 'T2') |
| `research_list_communications` | Список всех переписок исследования по его XRef идентификатору | `research_xref` (string) — XRef идентификатор исследования (например, 'R1') |
| `research_add_communication` | Добавить переписку в исследование по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `communication_xref` (string) — XRef идентификатор переписки (например, 'C1', 'C2') |
| `research_delete_communication` | Удалить переписку из исследования по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `communication_xref` (string) — XRef идентификатор переписки (например, 'C1', 'C2') |
| `research_list_groups` | Список всех групп исследования по его XRef идентификатору | `research_xref` (string) — XRef идентификатор исследования (например, 'R1') |
| `research_add_group` | Добавить группу в исследование по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `group_xref` (string) — XRef идентификатор группы (например, 'G1', 'G2') |
| `research_delete_group` | Удалить группу из исследования по их XRef идентификаторам | `research_xref` (string) — XRef идентификатор исследования (например, 'R1', 'R2'), `group_xref` (string) — XRef идентификатор группы (например, 'G1', 'G2') |

---

## Переписка (Communications)

| Инструмент | Описание | Параметры |
|---|---|---|
| `communication_upsert` | Добавить переписку или редактировать существующую | `xref` (string), `name` (string), `type` (enum), `direction` (enum), `corresponderXRef` (string), `date` (string) |

---

## Местоположения (Locations)

| Инструмент | Описание | Параметры |
|---|---|---|
| `location_upsert` | Добавить местоположение или редактировать существующее | `xref` (string), `name` (string), `lati` (number), `long` (number) |
| `location_list_names` | Список всех имён местоположения | `location_xref` (string, напр. 'L1') |
| `location_upsert_name` | Добавить имя к записи местоположения или редактировать существующее | `location_xref` (string, напр. 'L1'), `name_index` (integer, 0-based), `name` (string), `short_name` (string), `date` (string) |
| `location_delete_name` | Удалить имя из местоположения | `location_xref` (string, напр. 'L1'), `name_index` (integer, 0-based) |
| `location_list_top_links` | Список всех верхнеуровневых ссылок местоположения | `location_xref` (string, напр. 'L1') |
| `location_upsert_top_link` | Добавить верхнеуровневую ссылку к записи местоположения или редактировать существующую | `location_xref` (string, напр. 'L1'), `top_link_index` (integer, 0-based), `top_link_xref` (string), `date` (string) |
| `location_delete_top_link` | Удалить верхнеуровневую ссылку из местоположения | `location_xref` (string, напр. 'L1'), `top_link_index` (integer, 0-based) |

---

## Родословная (Pedigree)

| Инструмент | Описание | Параметры |
|---|---|---|
| `pedigree_traverse` | Обход родословной в заданном направлении от персоны | `individual_xref` (string), `direction` (string), `depth` (integer, по умолч. 1), `output_format` (string) |

---

## TODO

1. В разделе "Особенности реализации" указано, что нечеткий поиск имеет настраиваемый порог, но в описании инструмента `individual_search` указан фиксированный порог 16%, что создает путаницу.
2. Отсутствие инструментов для экспорта/импорта в других форматах.
3. Нет инструментов для расширенного поиска по датам, местам, событиям или комбинации критериев.
4. Нет инструментов для генерации статистики, построения диаграмм, анализа поколений и т.д.
5. Нет механизма отката изменений при частичной неудаче операции (например, при слиянии деревьев).
