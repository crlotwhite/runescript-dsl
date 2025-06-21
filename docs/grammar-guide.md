# RuneScript DSL 문법 가이드

이 문서는 RuneScript DSL의 문법에 대한 상세한 설명을 제공합니다.

## 📁 파일 타입 개요

### `.rcs` 파일 - 런스크립트 실행 파일
메인 실행 로직을 정의하는 파일입니다. 텍스트 처리 파이프라인을 선언적으로 정의할 수 있습니다.

### `.spell` 파일 - 사용자 정의 함수
사용자가 정의한 함수와 변환 로직을 포함합니다. G2P(Grapheme-to-Phoneme) 변환, 정규화 등의 기능을 정의할 수 있습니다.

### `.rune` 파일 - 문자 및 언어 정의
언어별 문자 집합, 유니코드 범위, 음성학적 변환 규칙을 정의합니다.

### `.caster` 파일 - 실행 전략 정의
파이프라인 실행 방식, 백엔드 설정, 성능 최적화 옵션을 정의합니다.

## 🖋️ `.rcs` 문법 상세

### 기본 구조

```runescript
// 코멘트
input = "Hello, World!"           // 변수 선언
result = input |> process()         // 파이프라인 정의
output result                       // 결과 출력
```

### 변수 선언

```runescript
// 문자열 변수
text = "안녕하세요"
pattern = /[\uac00-\ud7af]+/        // 정규식

// 목록
items = ["apple", "banana", "cherry"]

// 객체
config = {
  language: "ko-KR",
  model: "tacotron2"
}
```

### 파이프라인 연산자

```runescript
// 단순 파이프라인
result = input |> normalize() |> tokenize()

// 복잡한 파이프라인
processed = text 
  |> remove_punctuation()
  |> normalize_whitespace()
  |> korean_g2p()
  |> phoneme_to_mel()
```

### 함수 호출

```runescript
// 인수 없는 함수
clean_text = normalize()

// 인수 있는 함수
filtered = filter_by_language("ko-KR")
tokenized = split_by_delimiter(",")

// 다중 인수
replaced = replace_text("old", "new", "global")
```

### 모듈 가져오기

```runescript
// 모듈 가져오기
import "korean_g2p.spell"
import "english_processing.spell" as eng

// 네임스페이스 사용
result = text |> eng.normalize() |> eng.g2p()
```

## 🦄 `.spell` 문법 상세

### 함수 정의

```spell
// 기본 함수
def greet(name) = "Hello, " + name + "!"

// 파이프라인 함수
def process_korean(text) = 
  text |> remove_special_chars() |> hangul_normalize()

// 복합 함수
def full_g2p_pipeline(text) = 
  text 
    |> preprocess()
    |> korean_g2p()
    |> postprocess()
    |> validate_phonemes()
```

### 조건부 로직

```spell
def conditional_process(text, lang) = 
  if lang == "ko" then
    text |> korean_g2p()
  else if lang == "en" then
    text |> english_g2p()
  else
    text |> default_process()
```

### 리스트 처리

```spell
def process_list(items) = 
  map(items, normalize) |> filter(is_valid) |> sort()

def batch_g2p(texts) = 
  map(texts, korean_g2p) |> join_with(" ")
```

## 🔤 `.rune` 문법 상세

### 유니코드 범위 정의

```rune
// 한글 음절 블록
define block hangul_syllables = U+AC00 .. U+D7AF

// 한글 자모 블록
define block hangul_jamo = U+1100 .. U+11FF

// 이모티콘 블록
define block emoticons = U+1F600 .. U+1F64F
```

### 문자 그룹 정의

```rune
// 한글 모음
group korean_vowels = ["ㅡ", "ㅢ", "ㅣ", "ㅤ", "ㅥ", "ㅦ", "ㅧ", "ㅨ", "ㅩ", "ㅪ"]

// 한글 자음
group korean_consonants = ["ㄱ", "ㄲ", "ㄴ", "ㄵ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ"]

// 영어 모음
group english_vowels = ["a", "e", "i", "o", "u", "y"]
```

### 음성학적 변환 규칙

```rune
// 구개음화 규칙
rule palatalization: /ㄱㅣ/ => "ㅈㅣ"

// 비음화 규칙
rule nasalization: /ㅂㅁ/ => "ㅁㅁ"

// 연음 변화
rule liaison: /ㄹ ㅡ/ => "ㄴㅡ"

// 종성 중화
rule final_neutralization: /ㄱ$/ => "ㄷ"
```

### 언어 프로파일

```rune
// 한국어 프로파일
lang "ko-KR" uses hangul_syllables, hangul_jamo, korean_vowels, korean_consonants

// 영어 프로파일
lang "en-US" uses latin_extended, english_vowels, english_consonants

// 일본어 프로파일
lang "ja-JP" uses hiragana, katakana, kanji, japanese_punctuation
```

## ⚙️ `.caster` 문법 상세

### 모듈 로딩

```caster
// 필수 모듈
load "korean_g2p.spell"
load "korean.rune"
load "utils.spell"

// 조건부 로딩
load "advanced_features.spell" if ADVANCED_MODE
```

### 백엔드 설정

```caster
// 실행 백엔드 선택
use_backend "cpp_runtime"        // C++ 런타임
use_backend "python_interp"      // Python 인터프리터
use_backend "wasm_vm"            // WebAssembly VM
```

### 실행 단계 정의

```caster
// 순차적 단계
stage preprocessing: run "normalize_text"
stage tokenization: run "tokenize_by_language"
stage g2p_conversion: run "korean_g2p"
stage postprocessing: run "format_output"

// 병렬 단계
stage parallel_processing: run_parallel [
  "audio_feature_extraction",
  "linguistic_analysis"
]
```

### 런타임 설정

```caster
runtime {
    // 성능 설정
    threads = 4
    memory_limit = "2GB"
    timeout = 30000  // 30초
    
    // 최적화 옵션
    optimization_level = 2
    enable_vectorization = true
    cache_enabled = true
    
    // 디버깅
    debug_mode = false
    verbose_logging = false
    profile_execution = false
}
```

### 실행 스케줄

```caster
schedule {
    // 단계 순서
    pipeline_order = [
        "preprocessing", 
        "tokenization", 
        "g2p_conversion", 
        "postprocessing"
    ]
    
    // 병렬 실행 가능한 단계
    parallel_stages = ["g2p_conversion", "feature_extraction"]
    
    // 체크포인트 단계
    checkpoint_stages = ["g2p_conversion"]
    
    // 실패 시 재시도
    retry_on_failure = ["g2p_conversion"]
    max_retries = 3
}
```

## 📝 문법 베스트 프랙티스

### 1. 모듈화
- 기능별로 `.spell` 파일을 분리
- 언어별로 `.rune` 파일을 분리
- 재사용 가능한 단위로 구성

### 2. 네이밍 규칙
- 문자열: `snake_case` 사용
- 함수: `camelCase` 또는 `snake_case` 사용
- 상수: `UPPER_CASE` 사용

### 3. 코멘트
```runescript
// 단일 라인 코멘트
result = input |> process()  // 입력 처리

/*
 다중 라인 코멘트
 복잡한 로직에 대한 설명
*/
```

### 4. 에러 처리
```runescript
// 예외 처리 예제 (계획)
try {
    result = risky_operation()
} catch (error) {
    log_error(error)
    result = default_value
}
```

이 문서는 RuneScript DSL의 기본 문법을 다루며, 향후 버전에서 계속 업데이트될 예정입니다.
