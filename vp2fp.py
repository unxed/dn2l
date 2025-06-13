#!/usr/bin/python3
import re
import argparse
import os
import sys
import shutil

def process_pascal_file(filepath, encoding, create_backup):
    """
    Основная функция, которая читает, анализирует и преобразует Pascal-файл.
    """
    print(f"--- Начинаю обработку файла: {filepath} ---")

    # --- Шаг 0: Резервное копирование и чтение файла ---
    if create_backup:
        backup_path = filepath + ".bak"
        try:
            shutil.copy2(filepath, backup_path)
            print(f"  [OK] Создана резервная копия: {backup_path}")
        except Exception as e:
            print(f"[ОШИБКА] Не удалось создать резервную копию: {e}")
            sys.exit(1)
            
    try:
        # newline='' гарантирует, что переводы строк (CRLF/LF) не будут изменены
        with open(filepath, 'r', encoding=encoding, errors='replace', newline='') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"[ОШИБКА] Файл не найден: {filepath}")
        sys.exit(1)
    except Exception as e:
        print(f"[ОШИБКА] Не удалось прочитать файл: {e}")
        sys.exit(1)

    # Разделяем файл на две части: до 'implementation' и после.
    impl_keyword_match = re.search(r'\bimplementation\b', content, re.IGNORECASE)
    if not impl_keyword_match:
        print(f"[ИНФО] Ключевое слово 'implementation' не найдено. Файл не изменен.")
        return

    split_point = impl_keyword_match.start()
    header_and_interface_part = content[:split_point]
    implementation_part = content[split_point:]

    # --- Шаг 1: Парсинг блока interface ---
    print("\n[1] Анализ блока interface...")
    signatures = {}
    
    class_pattern = re.compile(
        r'^\s*(T\w+)\s*=\s*(?:class|object)\b[\s\S]*?\n\s*end;',
        re.IGNORECASE | re.MULTILINE
    )
    subprogram_pattern = re.compile(
        r'^\s*(?:procedure|function)\s+([a-zA-Z_]\w*)\s*([\s\S]*?;)',
        re.IGNORECASE | re.MULTILINE
    )

    # 1.1: Методы классов
    print("  Анализ методов классов...")
    interface_without_classes = header_and_interface_part
    for class_match in class_pattern.finditer(header_and_interface_part):
        class_name = class_match.group(1)
        class_body = class_match.group(0)
        print(f"  Найден класс: {class_name}")

        for method_match in subprogram_pattern.finditer(class_body):
            method_name = method_match.group(1)
            method_tail = method_match.group(2).strip()
            key = f"{class_name.lower()}.{method_name.lower()}"
            signatures[key] = method_tail
            print(f"    [+] Найдена сигнатура метода: {key} -> '{' '.join(method_tail.split())}'")
        
        interface_without_classes = interface_without_classes.replace(class_body, '')

    # 1.2: Отдельные процедуры и функции
    print("\n  Анализ отдельных процедур и функций...")
    found_standalone = False
    for func_match in subprogram_pattern.finditer(interface_without_classes):
        func_name = func_match.group(1)
        func_tail = func_match.group(2).strip()
        key = func_name.lower()
        signatures[key] = func_tail
        found_standalone = True
        print(f"    [+] Найдена сигнатура подпрограммы: {key} -> '{' '.join(func_tail.split())}'")
        
    if not found_standalone:
        print("    Отдельные подпрограммы не найдены.")

    if not signatures:
        print("\n[ИНФО] В блоке interface не найдено подпрограмм для преобразования. Файл не изменен.")
        return

    # --- Шаг 2 и 3: Поиск и замена в блоке implementation ---
    print("\n[2] Обработка блока implementation...")

    def replacer(match):
        qualified_name = match.group(1) # TMyObject.DoSomething или MyFunction
        key = qualified_name.lower()

        if key in signatures:
            full_short_declaration = match.group(0)
            base_header = full_short_declaration.rstrip()[:-1]
            full_header = base_header + signatures[key]
            
            print(f"  [*] Заменяю:  '{full_short_declaration.strip()}'")
            print(f"  [>] На:       '{' '.join(full_header.strip().split())}'")
            return full_header
        else:
            print(f"  [!] ВНИМАНИЕ: Для '{qualified_name}' не найдена сигнатура в interface. Строка не изменена.")
            return match.group(0)

    qualified_pattern = re.compile(
        r'^\s*(?:procedure|function)\s+((?:T\w+)\.\w+);',
        re.IGNORECASE | re.MULTILINE
    )
    standalone_pattern = re.compile(
        r'^\s*(?:procedure|function)\s+([a-zA-Z_][\w]*);',
        re.IGNORECASE | re.MULTILINE
    )

    processed_implementation = qualified_pattern.sub(replacer, implementation_part)
    new_implementation_part = standalone_pattern.sub(replacer, processed_implementation)
    
    # --- Шаг 4: Сборка и запись в исходный файл ---
    final_content = header_and_interface_part + new_implementation_part
    
    # Проверяем, изменился ли контент, чтобы не перезаписывать файл без надобности
    if final_content == content:
        print("\n--- Изменений не требуется. Файл остался прежним. ---")
        return

    try:
        # Перезаписываем исходный файл, сохраняя переводы строк (newline='')
        with open(filepath, 'w', encoding=encoding, newline='') as f:
            f.write(final_content)
    except Exception as e:
        print(f"[ОШИБКА] Не удалось записать изменения в файл: {e}")
        sys.exit(1)

    print(f"\n--- Преобразование завершено. Файл '{filepath}' был изменен. ---")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Конвертер Pascal-файлов из синтаксиса Virtual Pascal в Free Pascal. Изменяет файл на месте.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("pascal_file", help="Путь к исходному .pas файлу для конвертации.")
    parser.add_argument(
        "--backup",
        action="store_true",
        help="Создать резервную копию исходного файла с расширением .bak перед изменением."
    )
    parser.add_argument(
        "-e", "--encoding",
        default="cp1251",
        help="Кодировка исходного файла (например, cp1251, utf-8).\n"
             "Для старых Pascal-проектов часто используется 'cp1251'.\n"
             "По умолчанию: cp1251"
    )

    args = parser.parse_args()
    
    print("="*65)
    print("ВАЖНО: Этот скрипт изменяет исходный файл НА МЕСТЕ.")
    print("Настоятельно рекомендуется использовать флаг --backup для создания")
    print("резервной копии перед первым запуском на важных файлах.")
    print(f"Пример безопасного запуска: python {os.path.basename(__file__)} {args.pascal_file} --backup")
    print("="*65)
    
    process_pascal_file(args.pascal_file, args.encoding, args.backup)