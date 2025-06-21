# RuneScript DSL 예제 모음

이 디렉토리는 RuneScript DSL의 다양한 사용 예제들을 포함합니다.

## 📁 예제 디렉토리 구조

```
examples/
├── basic/                    # 기본 사용법 예제
│   ├── hello.rcs             # 간단한 Hello World 예제
│   ├── korean_g2p.spell      # 한글 G2P 함수 정의
│   ├── korean.rune           # 한글 문자 정의
│   └── pipeline.caster       # 실행 전략 설정
├── multilingual/             # 다국어 처리 예제
│   ├── mixed_text.rcs        # 한영 혼용 텍스트 처리
│   ├── language_detect.spell # 언어 감지 함수
│   └── multilang.rune        # 다언어 문자 정의
├── advanced/                 # 고급 기능 예제
│   ├── batch_processing.rcs  # 배치 처리
│   ├── custom_rules.spell    # 사용자 정의 규칙
│   └── optimization.caster  # 성능 최적화 설정
└── speech_synthesis/         # 음성 합성 예제
    ├── tts_pipeline.rcs      # TTS 파이프라인
    ├── phoneme_processing.spell # 음소 처리 함수
    └── voice_models.caster   # 음성 모델 설정
```

## 🚀 빠른 시작

### 1. 기본 예제 실행

```bash
# 기본 예제 실행
cd examples/basic
runescript hello.rcs

# 또는 caster 설정과 함께 실행
runescript --caster pipeline.caster hello.rcs
```

### 2. 다국어 처리 예제

```bash
cd examples/multilingual
runescript mixed_text.rcs
```

### 3. 고급 기능 예제

```bash
cd examples/advanced
runescript --caster optimization.caster batch_processing.rcs
```

## 📚 예제 상세 설명

### Basic Examples

#### `hello.rcs` - 기본 텍스트 처리
가장 기본적인 RuneScript 사용법을 보여줍니다:
- 변수 선언
- 파이프라인 연산자 사용
- 모듈 가져오기
- 기본 함수 호출

#### `korean_g2p.spell` - 한글 G2P 함수
한글 문자를 음소로 변환하는 함수들을 정의:
- 한글 음절 분해
- 음성학적 변환 규칙 적용
- IPA 표기로 변환

#### `korean.rune` - 한글 문자 정의
한글 처리에 필요한 문자 집합과 규칙들:
- 유니코드 블록 정의
- 모음/자음 그룹 정의
- 음성학적 변환 규칙

#### `pipeline.caster` - 실행 전략
효율적인 파이프라인 실행을 위한 설정:
- 모듈 로딩 순서
- 병렬 처리 설정
- 성능 최적화 옵션

### Multilingual Examples

#### `mixed_text.rcs` - 한영 혼용 텍스트
여러 언어가 섮인 텍스트를 처리하는 예제:
```runescript
input = "안녕하세요 Hello 세계 World!"

// 언어별로 분리 처리
korean_part = input |> extract_language("ko")
english_part = input |> extract_language("en")

// 각각 언어에 맞는 G2P 적용
korean_phonemes = korean_part |> korean_g2p()
english_phonemes = english_part |> english_g2p()

// 결과 합성
result = merge_phonemes(korean_phonemes, english_phonemes)
```

### Advanced Examples

#### `batch_processing.rcs` - 배치 처리
대량의 텍스트를 효율적으로 처리:
```runescript
// 파일 목록 읽기
input_files = read_file_list("./texts/*.txt")

// 병렬 처리
results = input_files 
  |> parallel_map(read_file)
  |> parallel_map(normalize)
  |> parallel_map(korean_g2p)
  |> collect_results()

// 결과 저장
results |> save_to_files("./output/")
```

### Speech Synthesis Examples

#### `tts_pipeline.rcs` - TTS 파이프라인
텍스트에서 음성까지의 전체 파이프라인:
```runescript
import "phoneme_processing.spell" as phoneme
import "voice_models.caster" as voice

// 입력 텍스트
input_text = "안녕하세요, RuneScript로 음성 합성을 해보세요."

// 텍스트 전처리
normalized = input_text 
  |> normalize_text()
  |> expand_abbreviations()
  |> handle_numbers()

// 음소 변환
phonemes = normalized 
  |> korean_g2p()
  |> phoneme.add_stress_marks()
  |> phoneme.add_tone_marks()

// 음성 특징 추출
mel_spectrogram = phonemes 
  |> text_to_mel(voice.get_model("korean_female"))
  |> apply_prosody()

// 음성 생성
audio = mel_spectrogram 
  |> mel_to_audio(voice.get_vocoder("hifigan"))
  |> normalize_audio()

// 결과 저장
audio |> save_audio("output.wav", sample_rate=22050)
```

## 🛠️ 실행 및 테스트

### 예제 실행

```bash
# 전체 예제 테스트
make test_examples

# 특정 예제만 실행
runescript examples/basic/hello.rcs
runescript examples/multilingual/mixed_text.rcs
runescript examples/speech_synthesis/tts_pipeline.rcs
```

### 성능 벤치마크

```bash
# 성능 측정
runescript --benchmark examples/advanced/batch_processing.rcs

# 메모리 사용량 모니터링
runescript --profile examples/speech_synthesis/tts_pipeline.rcs
```

## 📝 커스텀 예제 작성 가이드

### 1. 새로운 예제 추가

1. 적절한 디렉토리에 예제 파일들 생성
2. README에 예제 설명 추가
3. 테스트 케이스 작성

### 2. 베스트 프랙티스

- **주석**: 코드에 충분한 설명 추가
- **단계별 접근**: 복잡한 예제를 단계별로 분리
- **오류 처리**: 예상 가능한 오류에 대한 처리 방법 제시
- **성능 고려**: 성능 최적화 기법 시연

## 🔗 관련 링크

- [문법 가이드](../docs/grammar-guide.md)
- [API 참조](../docs/api-reference.md)
- [개발 계획서](../docs/development-plan.md)
- [기여 가이드](../CONTRIBUTING.md)

이 예제들을 통해 RuneScript DSL의 강력한 기능들을 경험해보세요! 🎩✨
