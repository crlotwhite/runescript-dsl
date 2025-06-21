# RuneScript DSL에 기여하기

RuneScript DSL 프로젝트에 관심을 가져주어 감사합니다! 이 문서는 기여 방법에 대한 가이드를 제공합니다.

## 🚀 시작하기

### 개발 환경 설정

1. **저장소 포크**
   ```bash
   git clone https://github.com/your-username/runescript-dsl.git
   cd runescript-dsl
   ```

2. **C++ 의존성 설치**
   - C++20 지원 컴파일러 (GCC 10+, Clang 11+, MSVC 2019+)
   - CMake 3.16+
   
3. **Haskell 의존성 설치** (선택적)
   ```bash
   # Stack 설치 (https://docs.haskellstack.org/)
   cd haskell/spell-checker
   stack build
   ```

4. **빌드 테스트**
   ```bash
   mkdir build && cd build
   cmake ..
   make
   make test
   ```

## 📝 기여 과정

### 1. 이슈 신고 및 논의

- **버그 리포트**: GitHub Issues를 통해 버그를 신고해주세요
- **기능 요청**: 새로운 기능에 대한 아이디어를 공유해주세요
- **논의**: Discussion 섹션에서 설계 결정에 대해 논의해주세요

### 2. 코드 기여

1. **브랜치 생성**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **코드 작성 및 테스트**
   - 코드 스타일 가이드를 따라주세요
   - 단위 테스트를 작성해주세요
   - 코드 커버리지를 유지해주세요

3. **커밋 및 푸시**
   ```bash
   git add .
   git commit -m "feat: add your feature description"
   git push origin feature/your-feature-name
   ```

4. **풀 리퀘스트 생성**
   - GitHub에서 PR을 생성해주세요
   - 변경 사항을 자세히 설명해주세요
   - 관련 이슈를 링크해주세요

## 📋 코드 스타일 가이드

### C++ 코드 스타일

- **명명 규칙**: `snake_case` 사용
- **클래스**: `PascalCase` 사용
- **네임스페이스**: `runescript::core`, `runescript::ast` 등
- **헤더 가드**: `#pragma once` 사용
- **인덴테이션**: 4칸 공백

```cpp
namespace runescript {
namespace core {

class Parser {
public:
    explicit Parser(const std::string& source);
    std::unique_ptr<ast::Program> parseRCS();
    
private:
    std::string source_;
    size_t position_;
};

} // namespace core
} // namespace runescript
```

### Haskell 코드 스타일

- **모듈 명**: `SpellChecker.Parser`, `SpellChecker.TypeInfer` 등
- **함수 명**: `camelCase` 사용
- **타입 명**: `PascalCase` 사용
- **인덴테이션**: 2칸 공백

```haskell
module SpellChecker.TypeInfer where

inferType :: TypeEnv -> Expr () -> Either TypeError Type
inferType env expr = case expr of
  EVar _ name -> lookupVar env name
  EString _ _ -> Right TText
  -- ...
```

## 🧪 테스트 가이드라인

### C++ 테스트

- **테스트 프레임워크**: Google Test 사용 계획
- **테스트 파일 위치**: `tests/` 디렉토리
- **명명 규칙**: `TestClassName_MethodName_ExpectedBehavior`

```cpp
TEST(ParserTest, ParseRCS_SimpleAssignment_ReturnsValidAST) {
    std::string source = "x = \"hello\"";
    Parser parser(source);
    auto result = parser.parseRCS();
    ASSERT_TRUE(result != nullptr);
}
```

### Haskell 테스트

- **테스트 프레임워크**: Hspec + QuickCheck
- **테스트 파일**: `test/` 디렉토리

```haskell
spec :: Spec
spec = describe "Type Inference" $ do
  it "infers text type for string literals" $
    inferType [] (EString () "hello") `shouldBe` Right TText
```

## 📚 문서화

- **코드 코멘트**: 복잡한 로직에 대한 설명 추가
- **API 문서**: 공개 API에 대한 Doxygen 코멘트
- **예제 코드**: `examples/` 디렉토리에 사용 예제 추가
- **README 업데이트**: 새로운 기능 추가 시 README 업데이트

## 🐛 버그 리포트

버그를 발견하셨다면 다음 정보를 포함해주세요:

- **운영 체제** 및 컴파일러 버전
- **재현 단계**: 버그를 재현하는 방법
- **예상 결과**: 어떤 결과를 기대했는지
- **실제 결과**: 실제로 어떤 일이 발생했는지
- **추가 대치**: 버그를 해결하기 위해 시도한 방법들

## 💬 커뮤니티

- **GitHub Discussions**: 일반적인 논의와 질문
- **Issues**: 버그 리포트와 기능 요청
- **Pull Requests**: 코드 리뷰와 기여

## 🎆 인정

기여해주신 모든 분들을 README의 Contributors 섹션에 추가하여 인정하겠습니다.

감사합니다! 함께 훌륭한 RuneScript DSL을 만들어가요! 🎩✨
