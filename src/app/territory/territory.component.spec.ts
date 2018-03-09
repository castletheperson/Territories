import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TerritoryComponent } from './territory.component';

describe('TerritoryComponent', () => {
  let component: TerritoryComponent;
  let fixture: ComponentFixture<TerritoryComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TerritoryComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TerritoryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
