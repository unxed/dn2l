#!/usr/bin/python3
import re
import argparse
import os
import sys
import shutil

def find_declarations(text):
    """
    Находит полные объявления procedure/function/constructor/destructor в тексте,
    корректно обрабатывая многострочные параметры и точки с запятой внутри них.
    """
    declarations = []
    # Ищем начало объявления: 'procedure', 'function', 'constructor' или 'destructor' в начале строки
    for match in re.finditer(r'^\s*(?:procedure|function|constructor|destructor)\s+\w', text, re.IGNORECASE | re.MULTILINE):
        start_index = match.start()
        
        if any(start_index >= decl_start and start_index < decl_end for decl_start, decl_end, _ in declarations):
            continue

        paren_level = 0
        end_index = -1
        
        for i in range(start_index, len(text)):
            char = text[i]
            if char == '(':
                paren_level += 1
            elif char == ')':
                paren_level -= 1
            elif char == ';' and paren_level == 0:
                end_index = i + 1
                break
        
        if end_index != -1:
            declarations.append((start_index, end_index, text[start_index:end_index]))

    return [decl_text for _, _, decl_text in declarations]

def process_pascal_file(filepath, encoding, create_backup):
    """
    Основная функция, которая читает, анализирует и преобразует Pascal-файл.
    """
    print(f"--- Начинаю обработку файла: {filepath} ---")

    if create_backup:
        backup_path = filepath + ".bak"
        try:
            shutil.copy2(filepath, backup_path)
            print(f"  [OK] Создана резервная копия: {backup_path}")
        except Exception as e:
            print(f"[ОШИБКА] Не удалось создать резервную копию: {e}")
            sys.exit(1)
            
    try:
        with open(filepath, 'r', encoding=encoding, errors='replace', newline='') as f:
            content = f.read()
    except Exception as e:
        print(f"[ОШИБКА] Не удалось прочитать файл: {e}")
        sys.exit(1)

    impl_keyword_match = re.search(r'\bimplementation\b', content, re.IGNORECASE)
    if not impl_keyword_match:
        print(f"[ИНФО] Ключевое слово 'implementation' не найдено. Файл не изменен.")
        return

    split_point = impl_keyword_match.start()
    header_and_interface_part = content[:split_point]
    implementation_part = content[split_point:]

    print("\n[1] Сбор сигнатур...")
    signatures = {}
    
    class_pattern = re.compile(
        r'^\s*(T\w+)\s*=\s*(?:class|object)\b[\s\S]*?\n\s*end;',
        re.IGNORECASE | re.MULTILINE
    )
    extractor_pattern = re.compile(
        r'^\s*(?:procedure|function|constructor|destructor)\s+([a-zA-Z_]\w*)\s*([\s\S]*)$',
        re.IGNORECASE
    )
    
    # --- 1.1: Анализ блока INTERFACE ---
    print("  Анализ блока interface...")
    interface_without_classes = header_and_interface_part
    for class_match in class_pattern.finditer(header_and_interface_part):
        class_name = class_match.group(1)
        class_body = class_match.group(0)
        print(f"    Найден public класс: {class_name}")

        declarations = find_declarations(class_body)
        for decl_text in declarations:
            match = extractor_pattern.match(decl_text)
            if match:
                method_name = match.group(1)
                method_tail = match.group(2).strip()
                key = f"{class_name.lower()}.{method_name.lower()}"
                signatures[key] = method_tail
                print(f"      [+] Найдена сигнатура: {key}")
        
        interface_without_classes = interface_without_classes.replace(class_body, '')
    
    # Отдельные подпрограммы могут быть только в interface
    standalone_declarations = find_declarations(interface_without_classes)
    if standalone_declarations:
        print("    Найдены отдельные подпрограммы...")
        for decl_text in standalone_declarations:
            match = extractor_pattern.match(decl_text)
            if match:
                func_name = match.group(1)
                func_tail = match.group(2).strip()
                key = func_name.lower()
                signatures[key] = func_tail
                print(f"      [+] Найдена сигнатура: {key}")

    # --- 1.2: Анализ блока IMPLEMENTATION на наличие локальных классов ---
    print("\n  Анализ блока implementation на наличие локальных классов...")
    found_local_classes = False
    for class_match in class_pattern.finditer(implementation_part):
        found_local_classes = True
        class_name = class_match.group(1)
        class_body = class_match.group(0)
        print(f"    Найден local класс: {class_name}")

        declarations = find_declarations(class_body)
        for decl_text in declarations:
            match = extractor_pattern.match(decl_text)
            if match:
                method_name = match.group(1)
                method_tail = match.group(2).strip()
                key = f"{class_name.lower()}.{method_name.lower()}"
                signatures[key] = method_tail
                print(f"      [+] Найдена сигнатура: {key}")
    
    if not found_local_classes:
        print("    Локальные классы не найдены.")

    if not signatures:
        print("\n[ИНФО] Не найдено ни одной сигнатуры для преобразования. Файл не изменен.")
        return

    # --- Шаг 2: Обработка блока implementation ---
    print("\n[2] Замена коротких объявлений в implementation...")

    def replacer(match):
        qualified_name = match.group(1)
        key = qualified_name.lower()

        if key in signatures:
            full_short_declaration = match.group(0)
            base_header = full_short_declaration.rstrip()[:-1]
            tail = signatures[key]
            full_header = base_header + (" " + tail if tail else tail)
            
            print(f"  [*] Заменяю:  '{full_short_declaration.strip()}'")
            print(f"  [>] На:       '{' '.join(full_header.strip().split())}'")
            return full_header
        else:
            print(f"  [!] ВНИМАНИЕ: Для '{qualified_name}' не найдена сигнатура. Строка не изменена.")
            return match.group(0)

    qualified_pattern = re.compile(
        r'^\s*(?:procedure|function|constructor|destructor)\s+((?:T\w+)\.\w+);',
        re.IGNORECASE | re.MULTILINE
    )
    standalone_pattern = re.compile(
        r'^\s*(?:procedure|function)\s+([a-zA-Z_][\w]*);',
        re.IGNORECASE | re.MULTILINE
    )

    processed_implementation = qualified_pattern.sub(replacer, implementation_part)
    new_implementation_part = standalone_pattern.sub(replacer, processed_implementation)
    
    final_content = header_and_interface_part + new_implementation_part
    
    if final_content == content:
        print("\n--- Изменений не требуется. Файл остался прежним. ---")
        return

    try:
        with open(filepath, 'w', encoding=encoding, newline='') as f:
            f.write(final_content)
    except Exception as e:
        print(f"[ОШИБКА] Не удалось записать изменения в файл: {e}")
        sys.exit(1)

    print(f"\n--- Преобразование завершено. Файл '{filepath}' был изменен. ---")


if __name__ == "__main__":
    # ... (код командной строки остался без изменений) ...
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
        default="cp866",
        help="Кодировка исходного файла (например, cp866, utf-8).\n"
             "Для старых Pascal-проектов часто используется 'cp866'.\n"
             "По умолчанию: cp866"
    )

    args = parser.parse_args()
    
    print("="*65)
    print("ВАЖНО: Этот скрипт изменяет исходный файл НА МЕСТЕ.")
    print("Настоятельно рекомендуется использовать флаг --backup для создания")
    print("резервной копии перед первым запуском на важных файлах.")
    print(f"Пример безопасного запуска: python {os.path.basename(__file__)} {args.pascal_file} --backup")
    print("="*65)
    
    process_pascal_file(args.pascal_file, args.encoding, args.backup)